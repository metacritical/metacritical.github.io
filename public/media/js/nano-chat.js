(function () {
  var CDN_URL = "https://cdn.jsdelivr.net/npm/@xenova/transformers@2.17.2";
  var MODEL_ID = "Xenova/distilgpt2";
  var MAX_NEW_TOKENS = 90;

  function $(id) {
    return document.getElementById(id);
  }

  function appendMessage(target, role, text) {
    var item = document.createElement("div");
    item.className = "nano-msg nano-msg-" + role;
    var who = document.createElement("div");
    who.className = "nano-msg-role";
    who.textContent = role === "user" ? "You" : "Nano";
    var body = document.createElement("div");
    body.className = "nano-msg-body";
    body.textContent = text;
    item.appendChild(who);
    item.appendChild(body);
    target.appendChild(item);
    target.scrollTop = target.scrollHeight;
  }

  function setStatus(el, text, isError) {
    el.textContent = text;
    el.classList.toggle("nano-status-error", !!isError);
  }

  function loadTransformersScript() {
    return new Promise(function (resolve, reject) {
      if (window.Transformers || window.transformers) {
        resolve();
        return;
      }
      var existing = document.querySelector("script[data-nano-transformers='1']");
      if (existing) {
        existing.addEventListener("load", function () { resolve(); });
        existing.addEventListener("error", function () { reject(new Error("Failed loading Transformers.js")); });
        return;
      }
      var script = document.createElement("script");
      script.src = CDN_URL;
      script.async = true;
      script.dataset.nanoTransformers = "1";
      script.onload = function () { resolve(); };
      script.onerror = function () { reject(new Error("Failed loading Transformers.js")); };
      document.head.appendChild(script);
    });
  }

  function extractAssistantText(prompt, output) {
    if (!output) return "";
    var text = "";
    if (Array.isArray(output) && output[0] && output[0].generated_text) {
      text = output[0].generated_text;
    } else if (typeof output === "string") {
      text = output;
    } else {
      text = String(output);
    }
    if (text.indexOf(prompt) === 0) {
      text = text.slice(prompt.length);
    }
    return text.trim();
  }

  document.addEventListener("DOMContentLoaded", function () {
    var app = $("nano-chat-app");
    if (!app) return;

    var initBtn = $("nano-init");
    var sendBtn = $("nano-send");
    var input = $("nano-input");
    var status = $("nano-status");
    var messages = $("nano-messages");

    var generator = null;
    var busy = false;
    var history = [];

    function setBusy(v) {
      busy = v;
      initBtn.disabled = v;
      sendBtn.disabled = v || !generator;
      input.disabled = v || !generator;
    }

    async function initModel() {
      if (generator || busy) return;
      try {
        setBusy(true);
        setStatus(status, "Loading Transformers.js...");
        await loadTransformersScript();
        var api = window.Transformers || window.transformers;
        if (!api || !api.pipeline) throw new Error("Transformers.js not available in browser");
        setStatus(status, "Loading nano model: " + MODEL_ID + " (first load can take time)...");
        generator = await api.pipeline("text-generation", MODEL_ID);
        setStatus(status, "Nano Chat ready.");
        sendBtn.disabled = false;
        input.disabled = false;
        input.focus();
      } catch (err) {
        setStatus(status, "Nano Chat failed to start: " + err.message, true);
      } finally {
        setBusy(false);
      }
    }

    async function sendMessage() {
      if (!generator || busy) return;
      var userText = input.value.trim();
      if (!userText) return;

      appendMessage(messages, "user", userText);
      history.push({ role: "user", text: userText });
      input.value = "";

      var promptHeader =
        "You are Nano Chat, a concise assistant for a programming blog.\n" +
        "Keep answers short, practical, and technical.\n\nConversation:\n";
      var convo = history.slice(-6).map(function (m) {
        return (m.role === "user" ? "User: " : "Assistant: ") + m.text;
      }).join("\n");
      var prompt = promptHeader + convo + "\nAssistant:";

      try {
        setBusy(true);
        setStatus(status, "Thinking...");
        var output = await generator(prompt, {
          max_new_tokens: MAX_NEW_TOKENS,
          temperature: 0.7,
          top_p: 0.92,
          do_sample: true
        });
        var assistantText = extractAssistantText(prompt, output);
        if (!assistantText) assistantText = "I could not produce a response. Please try again.";
        history.push({ role: "assistant", text: assistantText });
        appendMessage(messages, "assistant", assistantText);
        setStatus(status, "Nano Chat ready.");
      } catch (err) {
        setStatus(status, "Generation failed: " + err.message, true);
      } finally {
        setBusy(false);
      }
    }

    initBtn.addEventListener("click", initModel);
    sendBtn.addEventListener("click", sendMessage);
    input.addEventListener("keydown", function (event) {
      if (event.key === "Enter" && !event.shiftKey) {
        event.preventDefault();
        sendMessage();
      }
    });

    appendMessage(
      messages,
      "assistant",
      "Hi. Click \"Load Nano Model\" to start local in-browser chat."
    );
  });
})();
