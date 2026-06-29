/**
 * Cloudflare Worker for story claps using D1 (SQLite).
 *
 * One-clap-per-visitor: each visitor can clap once per page. Clicking
 * again does nothing. Visitors are identified by a UUID generated
 * client-side and stored in localStorage.
 *
 * Anti-abuse layers:
 *   1. Visitor UUID (localStorage) — one clap per browser per page
 *   2. IP rate limiting (CF-Connecting-IP) — max 20 requests per IP per page
 *
 * Endpoints:
 *   GET  /api/clap?slug=<slug>&visitor=<uuid>  -> { ok, slug, count, clapped }
 *   POST /api/clap  { slug, visitor_id }       -> { ok, slug, count, clapped }
 *
 * Setup:
 *   1. wrangler d1 create sds
 *   2. wrangler d1 execute sds --command "
 *        CREATE TABLE IF NOT EXISTS claps (
 *          slug TEXT PRIMARY KEY,
 *          count INTEGER NOT NULL DEFAULT 0
 *        );
 *        CREATE TABLE IF NOT EXISTS visitor_claps (
 *          visitor_id TEXT,
 *          slug TEXT,
 *          PRIMARY KEY (visitor_id, slug)
 *        );"
 *   3. Bind as `DB` in wrangler.jsonc, then wrangler deploy.
 */

const MAX_REQUESTS_PER_IP = 20;

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);
    const corsHeaders = {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type",
    };

    if (request.method === "OPTIONS") {
      return new Response(null, { status: 204, headers: corsHeaders });
    }

    if (url.pathname !== "/api/clap") {
      return json({ ok: false, error: "Not found" }, 404, corsHeaders);
    }

    const db = env.DB;
    if (!db) {
      return json({ ok: false, error: "D1 database not bound" }, 500, corsHeaders);
    }

    try {
      if (request.method === "GET") {
        const slug = sanitizeSlug(url.searchParams.get("slug"));
        const visitorId = sanitizeVisitor(url.searchParams.get("visitor"));
        if (!slug) {
          return json({ ok: false, error: "slug is required" }, 400, corsHeaders);
        }
        const count = await getCount(db, slug);
        const clapped = visitorId ? await hasClapped(db, visitorId, slug) : false;
        return json({ ok: true, slug, count, clapped }, 200, corsHeaders);
      }

      if (request.method === "POST") {
        let body = {};
        try {
          body = await request.json();
        } catch (_) {}
        const slug = sanitizeSlug(body.slug);
        const visitorId = sanitizeVisitor(body.visitor_id);
        if (!slug || !visitorId) {
          return json({ ok: false, error: "slug and visitor_id are required" }, 400, corsHeaders);
        }

        const ip = request.headers.get("CF-Connecting-IP") || "unknown";
        const ipToggles = await getIpToggleCount(db, ip, slug);
        if (ipToggles >= MAX_REQUESTS_PER_IP) {
          return json({ ok: false, error: "Rate limit exceeded" }, 429, corsHeaders);
        }

        const result = await addClap(db, slug, visitorId, ip);
        return json({ ok: true, slug, count: result.count, clapped: result.clapped }, 200, corsHeaders);
      }

      return json({ ok: false, error: "Method not allowed" }, 405, corsHeaders);
    } catch (err) {
      return json({ ok: false, error: String(err.message || err) }, 500, corsHeaders);
    }
  },
};

function json(payload, status = 200, extraHeaders = {}) {
  return new Response(JSON.stringify(payload), {
    status,
    headers: { "Content-Type": "application/json", ...extraHeaders },
  });
}

function sanitizeSlug(raw) {
  return String(raw || "")
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9_-]+/g, "-")
    .replace(/-+/g, "-")
    .replace(/^-|-$/g, "");
}

function sanitizeVisitor(raw) {
  const v = String(raw || "").trim();
  return /^[a-f0-9-]{30,}$/i.test(v) ? v : "";
}

async function getCount(db, slug) {
  const result = await db
    .prepare("SELECT count FROM claps WHERE slug = ?")
    .bind(slug)
    .first();
  return result ? result.count : 0;
}

async function hasClapped(db, visitorId, slug) {
  const result = await db
    .prepare("SELECT 1 FROM visitor_claps WHERE visitor_id = ? AND slug = ?")
    .bind(visitorId, slug)
    .first();
  return !!result;
}

async function getIpToggleCount(db, ip, slug) {
  const result = await db
    .prepare("SELECT count FROM ip_toggle_log WHERE ip = ? AND slug = ?")
    .bind(ip, slug)
    .first();
  return result ? result.count : 0;
}

async function addClap(db, slug, visitorId, ip) {
  const already = await hasClapped(db, visitorId, slug);
  if (already) {
    return { count: await getCount(db, slug), clapped: true };
  }

  await db.batch([
    db.prepare(
      `INSERT INTO claps (slug, count) VALUES (?, 1)
       ON CONFLICT(slug) DO UPDATE SET count = count + 1`
    ).bind(slug),
    db.prepare("INSERT OR IGNORE INTO visitor_claps (visitor_id, slug) VALUES (?, ?)")
      .bind(visitorId, slug),
    db.prepare(
      `INSERT INTO ip_toggle_log (ip, slug, count) VALUES (?, ?, 1)
       ON CONFLICT(ip, slug) DO UPDATE SET count = count + 1`
    ).bind(ip, slug),
  ]);
  return { count: await getCount(db, slug), clapped: true };
}
