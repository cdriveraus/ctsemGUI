(function () {
  "use strict";

  $(function () {
    var app = $("#ctgui-app");
    if (!app.length) return;

    app.on("click", ".arg-help", function (event) {
      event.stopPropagation();
    });

    // Selectize normally focuses its search field first and opens on the
    // next click in some browser/input-binding combinations.  Opening after
    // the native mousedown keeps every variable selector one-click while
    // retaining normal keyboard creation and comma-separated entry.
    app.on("mousedown", ".selectize-control", function (event) {
      if ($(event.target).closest(".remove").length) return;
      var select = $(this).closest(".shiny-input-container").find("select")[0];
      var selectize = select && select.selectize;
      if (!selectize || selectize.isLocked) return;
      window.setTimeout(function () {
        selectize.focus();
        selectize.open();
      }, 0);
    });

    function specificationPayload() {
      var payload = {};
      [
        "latent_names", "manifest_names", "tdpred_names", "tipred_names",
        "id", "time", "type", "tipredDefault"
      ].forEach(function (id) {
        var control = app.find("#" + id);
        if (!control.length) return;
        payload[id] = control.is(":checkbox")
          ? control.prop("checked")
          : control.val();
      });
      app.find("[id^=\"manifest_type_\"]").each(function () {
        payload[this.id] = $(this).val();
      });
      return payload;
    }

    app.on("click", ".ctgui-spec-add", function () {
      var kind = $(this).attr("data-add-role");
      var control = app.find("#" + kind + "_names")[0];
      var selectize = control && control.selectize;
      var dataColumns = app.find("#ctgui-spec-data-choices option").map(function () { return this.value; }).get();
      var current = selectize ? (selectize.items || []) : [];
      var latentText = String(app.find("#latent_names").val() || "");
      var latentNames = latentText.split(",").map(function (name) { return name.trim(); }).filter(Boolean);
      var manifestNames = String(app.find("#manifest_names").val() || "").split(",").map(function (name) { return name.trim(); }).filter(Boolean);
      var excluded = current.concat([String(app.find("#id").val() || "")]);
      if (kind === "manifest") excluded = excluded.concat(manifestNames, [String(app.find("#time").val() || "")]);
      if (!window.ctguiVariableEntryDialog) return;
      window.ctguiVariableEntryDialog({
        host: app[0], id: "specification", kind: kind,
        used: current, dataColumns: dataColumns, excluded: excluded, latentNames: latentNames,
        callback: function (name, measuring) {
          if (window.Shiny) Shiny.setInputValue("spec_add_variable", {
            kind: kind, name: name, measuring: measuring, nonce: Math.random()
          }, { priority: "event" });
        }
      });
    });

    app.on("show.bs.tab", "a[data-toggle=\"tab\"]", function () {
      if (window.Shiny) {
        // Bootstrap starts changing tabs before Shiny is guaranteed to flush
        // the control that just lost focus. Carry the authored values in the
        // same event so the server never rebuilds from stale input bindings.
        Shiny.setInputValue("tab_commit_nonce", {
          nonce: Math.random(),
          specification: specificationPayload()
        }, { priority: "event" });
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
    app.on("change", ".matrix-cell input[type=\"text\"]", function (event) {
      var cell = $(this).closest(".matrix-cell");
      // updateTextInput() synchronizes widgets after a canonical commit and
      // can emit a synthetic change. Only native user edits start commits.
      if (!event.originalEvent) return;
      if (window.Shiny && cell.length) {
        Shiny.setInputValue("matrix_selected_cell", {
          matrix: cell.data("matrix"),
          row: cell.data("row"),
          col: cell.data("col")
        }, { priority: "event" });
        // Keep the edited value and its commit signal in one message. A
        // document-level Shiny input binding may otherwise deliver the old
        // value after this delegated change handler has requested a rebuild.
        Shiny.setInputValue("matrix_commit_nonce", {
          nonce: Math.random(),
          id: this.id,
          value: $(this).val()
        }, { priority: "event" });
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
