// Post-processes rendered R code blocks to color package names that
// precede `::`/`:::` (e.g. `bslib::page_navbar`) separately from
// ordinary function calls. Pandoc's highlighter has no dedicated token
// for R namespaces, so this recovers the distinction client-side by
// looking for a plain-text node immediately followed by a `.sc` span
// whose text is `::` or `:::`.
(function () {
  function highlightNamespaces(root) {
    var scSpans = root.querySelectorAll("code.sourceCode.r span.sc, code.sourceCode.r span.sc + span.sc");
    root.querySelectorAll("code.sourceCode.r").forEach(function (code) {
      var walker = document.createTreeWalker(code, NodeFilter.SHOW_TEXT);
      var textNodes = [];
      var node;
      while ((node = walker.nextNode())) {
        textNodes.push(node);
      }
      textNodes.forEach(function (textNode) {
        var match = /([a-zA-Z_.][a-zA-Z_.0-9]*)$/.exec(textNode.nodeValue);
        if (!match) return;
        var next = textNode.nextSibling;
        if (!next || next.nodeType !== 1 || !next.classList.contains("sc")) return;
        if (!/^:::?$/.test(next.textContent)) return;

        var name = match[1];
        var prefix = textNode.nodeValue.slice(0, textNode.nodeValue.length - name.length);
        textNode.nodeValue = prefix;

        var span = document.createElement("span");
        span.className = "dt namespace";
        span.textContent = name;
        next.parentNode.insertBefore(span, next);
      });
    });
  }

  function run() {
    highlightNamespaces(document);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", run);
  } else {
    run();
  }
  // codewindow.js runs on Reveal's `ready` event and rearranges code
  // nodes into tabs; run again afterward so those blocks get covered too.
  window.addEventListener("ready", run);
})();
