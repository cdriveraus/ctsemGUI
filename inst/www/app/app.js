(function () {
  "use strict";

  $(function () {
    var app = $("#ctgui-app");
    if (!app.length) return;

    app.on("click", ".arg-help", function (event) {
      event.stopPropagation();
    });

    app.on("show.bs.tab", "a[data-toggle=\"tab\"]", function () {
      if (window.Shiny) {
        Shiny.setInputValue("tab_commit_nonce", Math.random(), { priority: "event" });
      }
    });

    app.on("focusin click", ".matrix-cell input[type=\"text\"]", function () {
      var cell = $(this).closest(".matrix-cell");
      if (window.Shiny && cell.length) {
        Shiny.setInputValue("matrix_selected_cell", {
          matrix: cell.data("matrix"),
          row: cell.data("row"),
          col: cell.data("col")
        }, { priority: "event" });
      }
    });

    // Matrix text edits are saved on blur/change. This is especially
    // important when a fixed numeric value becomes a new free label: the
    // server must create its metadata row before the inspector can render.
    app.on("change", ".matrix-cell input[type=\"text\"]", function () {
      var cell = $(this).closest(".matrix-cell");
      if (window.Shiny && cell.length) {
        Shiny.setInputValue("matrix_selected_cell", {
          matrix: cell.data("matrix"),
          row: cell.data("row"),
          col: cell.data("col")
        }, { priority: "event" });
        window.setTimeout(function () {
          Shiny.setInputValue("matrix_commit_nonce", Math.random(), { priority: "event" });
        }, 0);
      }
    });

    var matrixMetadataTimer;
    app.on(
      "change",
      ".matrix-cell-inspector:not(.visual-path-inspector) input, " +
        ".matrix-cell-inspector:not(.visual-path-inspector) select",
      function () {
        clearTimeout(matrixMetadataTimer);
        matrixMetadataTimer = window.setTimeout(function () {
          if (window.Shiny) {
            Shiny.setInputValue("matrix_metadata_commit", Math.random(), { priority: "event" });
          }
        }, 100);
      }
    );

    var visualPathTimer;
    // Native change fires when edited text is blurred or tabbed away and
    // immediately for selectors/checkboxes. Avoid focusout itself: Shiny
    // can remove an inspector during rendering, and that focus loss must
    // not commit unchanged values and start a model-rebuild loop.
    app.on(
      "change",
      ".visual-path-inspector input, .visual-path-inspector select",
      function () {
        clearTimeout(visualPathTimer);
        visualPathTimer = window.setTimeout(function () {
          if (window.Shiny) {
            var payload = { nonce: Math.random() };
            app.find(".visual-path-inspector input, .visual-path-inspector select").each(function () {
              if (!this.id) return;
              if (this.type === "checkbox") payload[this.id] = this.checked;
              else payload[this.id] = $(this).val();
            });
            Shiny.setInputValue("visual_path_commit", payload, { priority: "event" });
          }
        }, 50);
      }
    );

    var workflow = app.find("#workflow");
    var dataTab = workflow.children("ul.nav").find("a[data-value=\"Data\"]").parent();
    if (dataTab.length) dataTab.prependTo(workflow.children("ul.nav"));

    app.on("click", "#run_fit", function () {
      app.find("input, select, textarea, button").not("#run_fit").prop("disabled", true);
      app.find("#run_fit").prop("disabled", true).text("Fitting...");
    });

    if (window.Shiny) {
      Shiny.addCustomMessageHandler("ctgui-fit-finished", function (message) {
        app.find("input, select, textarea, button").prop("disabled", false);
        app.find("#run_fit").text("Fit model");
        if (message.beep) {
          try {
            var context = new (window.AudioContext || window.webkitAudioContext)();
            var oscillator = context.createOscillator();
            oscillator.connect(context.destination);
            oscillator.start();
            oscillator.stop(context.currentTime + 0.15);
          } catch (error) {
            // Audio feedback is optional; fitting remains complete if unavailable.
          }
        }
      });
    }
  });
}());
