(function () {
  "use strict";

  // Model equations arrive from R as labelled TeX fragments, one per row of
  // the ctsem equation display, and are typeset here rather than compiled to
  // an image by a TeX installation the user is unlikely to have.

  var MACROS = {};
  MACROS["\\vect"] = "\\boldsymbol{#1}";

  function renderOne(element) {
    if (element.getAttribute("data-ctgui-rendered") === "1") return;
    var tex = element.getAttribute("data-ctgui-tex");
    if (!tex) return;
    element.setAttribute("data-ctgui-rendered", "1");

    if (!window.katex) {
      showSource(element, tex, "The equation renderer did not load.");
      return;
    }
    try {
      window.katex.render(tex, element, {
        displayMode: true,
        throwOnError: true,
        strict: false,
        trust: false,
        macros: MACROS
      });
    } catch (error) {
      // A fragment ctsem can emit but KaTeX cannot typeset is worth showing as
      // source: the equation is still readable and the user can copy it into a
      // TeX document, which a silent gap would not allow.
      showSource(element, tex, error && error.message ? error.message : String(error));
    }
  }

  function showSource(element, tex, message) {
    element.textContent = "";
    element.classList.add("ctgui-equation-unrendered");

    var note = document.createElement("p");
    note.className = "ctgui-equation-note";
    note.textContent = "Showing LaTeX source instead: " + message;

    var source = document.createElement("pre");
    source.textContent = tex;

    element.appendChild(note);
    element.appendChild(source);
  }

  function renderWithin(root) {
    if (!root || !root.querySelectorAll) return;
    var pending = root.querySelectorAll("[data-ctgui-tex]");
    for (var i = 0; i < pending.length; i++) renderOne(pending[i]);
  }

  // Zoom stays on the client. The scale is a reading preference, and routing
  // it through the server would recompute the ctsem equations on every step
  // of the slider.
  function applyScale(pane, value) {
    var scale = parseFloat(value);
    if (!pane || !isFinite(scale) || scale <= 0) return;
    pane.style.setProperty("--ctgui-equation-scale", String(scale));
  }

  function paneFor(input) {
    var container = input.closest(".tab-pane") || document;
    return container.querySelector(".equation-pane");
  }

  $(function () {
    var app = $("#ctgui-app");
    if (!app.length) return;

    renderWithin(app[0]);

    // uiOutput replaces its contents wholesale, so newly delivered fragments
    // have to be typeset as each output lands rather than once at startup.
    app.on("shiny:value", function (event) {
      window.setTimeout(function () {
        var target = document.getElementById(event.name || event.target.id);
        renderWithin(target || app[0]);
      }, 0);
    });

    app.on("input change", "#equation_zoom, #fit_equation_zoom", function () {
      applyScale(paneFor(this), $(this).val());
    });

    app.find("#equation_zoom, #fit_equation_zoom").each(function () {
      applyScale(paneFor(this), $(this).val());
    });
  });
}());
