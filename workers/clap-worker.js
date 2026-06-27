/**
 * Cloudflare Worker for persisting story claps in D1.
 *
 * Endpoints:
 *   GET  /api/clap?slug=<page-slug>  -> { ok, slug, count }
 *   POST /api/clap                   -> { ok, slug, count }
 *        body: { slug: "page-slug" }
 *
 * Setup:
 *   1. Create a D1 database in the Cloudflare dashboard.
 *   2. Run the schema:
 *        CREATE TABLE IF NOT EXISTS claps (
 *          slug TEXT PRIMARY KEY,
 *          count INTEGER NOT NULL DEFAULT 0
 *        );
 *   3. Bind the database to this Worker as `DB`.
 *   4. Deploy this worker and set window.SELF_DOTSEND_CLAP_API to its URL
 *      in your site's JS, e.g.:
 *        window.SELF_DOTSEND_CLAP_API = "https://claps.selfdotsend.workers.dev/api/clap";
 */

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
        if (!slug) {
          return json({ ok: false, error: "slug is required" }, 400, corsHeaders);
        }
        const count = await getCount(db, slug);
        return json({ ok: true, slug, count }, 200, corsHeaders);
      }

      if (request.method === "POST") {
        let body = {};
        try {
          body = await request.json();
        } catch (_) {}
        const slug = sanitizeSlug(body.slug);
        if (!slug) {
          return json({ ok: false, error: "slug is required" }, 400, corsHeaders);
        }
        const count = await incrementCount(db, slug);
        return json({ ok: true, slug, count }, 200, corsHeaders);
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

async function getCount(db, slug) {
  const result = await db
    .prepare("SELECT count FROM claps WHERE slug = ?")
    .bind(slug)
    .first();
  return result ? result.count : 0;
}

async function incrementCount(db, slug) {
  await db
    .prepare(
      `INSERT INTO claps (slug, count) VALUES (?, 1)
       ON CONFLICT(slug) DO UPDATE SET count = count + 1`
    )
    .bind(slug)
    .run();
  return getCount(db, slug);
}
