/* ctsemGUI visual specification editor.
 * Uses Cytoscape.js (MIT). Interaction design is independently implemented
 * for ctsemGUI, with acknowledgement to lavaangui for the general idea of a
 * browser-native SEM diagram editor (GPL-3.0-or-later).
 */
(function () {
  var editors = {};
  var GRAPH_PROTOCOL_VERSION = 1;
  var GRAPH_VERSION = 2;

  function makeElement(tagName, className, text) {
    var element = document.createElement(tagName);
    if (className) element.className = className;
    if (text !== undefined) element.textContent = text;
    return element;
  }

  function clearElement(element) {
    while (element.firstChild) element.removeChild(element.firstChild);
  }

  function isSafeColour(colour) {
    return typeof colour === "string" && /^#[0-9a-f]{6}$/i.test(colour);
  }

  function validGraph(graph) {
    return !!graph && typeof graph === "object" &&
      Number(graph.protocol_version) === GRAPH_PROTOCOL_VERSION &&
      Number(graph.version) === GRAPH_VERSION &&
      ["state_space", "initial_state", "tipred_effects"].indexOf(graph.view) >= 0 &&
      Array.isArray(graph.nodes) && Array.isArray(graph.edges) &&
      graph.nodes.every(function (node) { return node && typeof node.id === "string"; }) &&
      graph.edges.every(function (edge) { return edge && typeof edge.id === "string"; });
  }

  function cssClass(edge) {
    if (edge.visual_only && edge.edge_kind === "noise_input") return "directed noise_input";
    var classes = [edge.directed ? "directed" : "undirected", edge.edge_kind || "path"];
    classes.push(edge.fixed ? "fixed" : "free");
    if (edge.custom) classes.push("custom");
    if (edge.inactive) classes.push("inactive");
    if (edge.nonlinear) classes.push("nonlinear");
    if (edge.indvarying) classes.push("random");
    if (edge.tipred_effects && edge.tipred_effects.length) classes.push("moderated");
    if (edge.edge_kind === "tipred_effect") classes.push("tipred-effect");
    return classes.join(" ");
  }

  function semanticPosition(editor, kind, index, id) {
    if (editor.view === "initial_state") {
      if (kind === "latent") return { x: 360 + index * 190, y: 275 };
      if (kind === "initial_noise") return { x: 360 + index * 190, y: 95 };
      if (kind === "constant") return { x: 105, y: 180 };
      return { x: 140, y: 275 };
    }
    if (kind === "latent") return { x: 340 + index * 190, y: 255 };
    if (kind === "manifest") return { x: 340 + index * 190, y: 430 };
    if (kind === "system_noise") return { x: 340 + index * 190, y: 85 };
    if (kind === "measurement_noise") return { x: 340 + index * 190, y: 570 };
    if (kind === "tdpred") return { x: 85, y: 150 + index * 105 };
    if (kind === "tipred") return { x: 85, y: 430 + index * 85 };
    if (kind === "constant") return { x: 1080, y: id.indexOf("MANIFEST") >= 0 ? 505 : 180 };
    return { x: 180, y: 180 + index * 80 };
  }

  function serialise(editor) {
    var cy = editor.cy;
    return {
      protocol_version: GRAPH_PROTOCOL_VERSION,
      version: GRAPH_VERSION,
      view: editor.view,
      nodes: cy.nodes().filter(function (node) { return !node.data("preview"); }).map(function (node) {
        var data = Object.assign({}, node.data());
        var position = node.position(); data.x = position.x; data.y = position.y;
        return data;
      }),
      edges: cy.edges().filter(function (edge) { return !edge.data("preview"); }).map(function (edge) { return Object.assign({}, edge.data()); })
    };
  }

  function send(editor, changed, layoutOnly) {
    var graph = serialise(editor);
    graph.changed = !!changed; graph.layout_only = !!layoutOnly; graph.nonce = Math.random();
    Shiny.setInputValue(editor.id + "_graph", graph, { priority: "event" });
  }

  function select(editor, element) {
    if (element && element.isNode && element.isNode() && element.data("kind") === "parameter") {
      Shiny.setInputValue(editor.id + "_selection", Object.assign({ view: editor.view, parameter_node: true }, element.data()), { priority: "event" }); return;
    }
    if (element && element.isNode && element.isNode() && element.data("kind") === "tipred") {
      Shiny.setInputValue(editor.id + "_selection", Object.assign({ view: editor.view, tipred_node: true }, element.data()), { priority: "event" }); return;
    }
    if (!element || !element.isEdge || !element.isEdge() || element.data("visual_only")) {
      Shiny.setInputValue(editor.id + "_selection", null, { priority: "event" }); return;
    }
    var value = Object.assign({}, element.data());
    Shiny.setInputValue(editor.id + "_selection", value, { priority: "event" });
  }

  function uniqueName(editor, prefix) {
    var used = editor.cy.nodes().map(function (node) { return node.data("name"); });
    var i = 1, name = prefix + i;
    while (used.indexOf(name) >= 0) name = prefix + (++i);
    return name;
  }

  function variableEntryDialog(options) {
    var kind = options.kind, callback = options.callback;
    var overlay = makeElement("div", "ctgui-visual-dialog-backdrop");
    var dialog = makeElement("div", "ctgui-visual-dialog");
    var title = kind === "latent" ? "Add latent process" : kind === "manifest" ? "Add manifest variable" : kind === "tdpred" ? "Add time-dependent predictor" : "Add time-independent predictor";
    var excluded = options.excluded || [];
    var available = (options.dataColumns || []).filter(function (name) {
      return excluded.indexOf(name) < 0;
    });
    dialog.appendChild(makeElement("h5", "", title));
    var comboInput = function (label, names, placeholder) {
      var choiceLabel = makeElement("label", "", label);
      var combo = makeElement("div", "ctgui-combo");
      var input = makeElement("input"); input.type = "text"; input.placeholder = placeholder; input.autocomplete = "off";
      var menu = makeElement("div", "ctgui-combo-menu");
      var renderChoices = function () {
        clearElement(menu);
        var query = input.value.trim().toLowerCase();
        var matches = names.filter(function (name) { return !query || String(name).toLowerCase().indexOf(query) >= 0; });
        matches.forEach(function (name) {
          var option = makeElement("button", "ctgui-combo-option", String(name));
          option.type = "button";
          option.addEventListener("mousedown", function (event) { event.preventDefault(); });
          option.addEventListener("click", function () { input.value = String(name); menu.classList.remove("is-open"); input.focus(); });
          menu.appendChild(option);
        });
        menu.classList.toggle("is-open", matches.length > 0);
      };
      input.addEventListener("focus", renderChoices);
      input.addEventListener("click", renderChoices);
      input.addEventListener("input", renderChoices);
      input.addEventListener("keydown", function (event) { if (event.key === "Escape") menu.classList.remove("is-open"); });
      combo.appendChild(input); combo.appendChild(menu); choiceLabel.appendChild(combo); dialog.appendChild(choiceLabel);
      return input;
    };
    var nameInput = comboInput(
      kind === "latent" ? "Latent process name" : "Dataset variable or new name",
      kind === "latent" ? [] : available,
      kind === "latent" ? "Latent process name" : "Choose or type a variable name"
    );
    var latentInput = null;
    if (kind === "manifest") {
      var latentNames = options.latentNames || [];
      latentInput = comboInput(
        "Measuring which latent", latentNames,
        latentNames.length ? "Choose or type a latent process" : "Type a latent process name"
      );
    }
    var actions = makeElement("div", "ctgui-visual-dialog-actions");
    var cancel = addButton(actions, "Cancel", "data-dialog", "cancel");
    var add = addButton(actions, "Add", "data-dialog", "add");
    var close = function () { overlay.remove(); };
    cancel.addEventListener("click", close);
    add.addEventListener("click", function () {
      var name = nameInput.value.trim();
      var measuring = latentInput ? latentInput.value.trim() : "";
      close(); callback(name, measuring);
    });
    nameInput.addEventListener("keydown", function (event) { if (event.key === "Enter") add.click(); });
    dialog.appendChild(actions); overlay.appendChild(dialog); options.host.appendChild(overlay);
  }

  window.ctguiVariableEntryDialog = variableEntryDialog;

  function visualVariableDialog(editor, kind, prefix, callback) {
    var roles = editor.dataRoles || {};
    var roleList = function (value) {
      if (Array.isArray(value)) return value;
      return value ? [value] : [];
    };
    var excluded = roleList(roles[kind]).slice();
    excluded = excluded.concat(roleList(roles.id));
    if (kind === "manifest") excluded = excluded.concat(roleList(roles.time));
    variableEntryDialog({
      host: editor.shell, id: editor.id, kind: kind,
      used: editor.cy.nodes().map(function (node) { return node.data("name"); }),
      dataColumns: editor.dataColumns,
      excluded: excluded,
      latentNames: editor.cy.nodes("node.latent").map(function (node) { return node.data("name"); }),
      callback: callback
    });
  }

  function addVariable(editor, kind, prefix) {
    var create = function (name, measuring) {
      if (!name) return;
      name = name.trim();
      if (!/^[A-Za-z._][A-Za-z0-9._]*$/.test(name)) { window.alert("Use a valid R variable name."); return; }
      if (editor.cy.nodes().some(function (node) { return node.data("name") === name; })) { window.alert("Names must be unique."); return; }
      var latent = null;
      if (kind === "manifest" && measuring) {
        if (!/^[A-Za-z._][A-Za-z0-9._]*$/.test(measuring)) { window.alert("Use a valid R variable name for the latent process."); return; }
        latent = editor.cy.getElementById("latent:" + measuring);
        if (!latent.length) {
          var latentIndex = editor.cy.nodes("node.latent").length;
          editor.cy.add({ group: "nodes", classes: "latent", data: { id: "latent:" + measuring, kind: "latent", name: measuring, label: measuring, original_name: measuring }, position: semanticPosition(editor, "latent", latentIndex, "latent:" + measuring) });
          latent = editor.cy.getElementById("latent:" + measuring);
        }
      }
      var index = editor.cy.nodes("node." + kind).length;
      var data = { id: kind + ":" + name, kind: kind, name: name, label: name, original_name: name };
      if (kind === "tipred") {
        data.tipred_default = window.confirm("Should " + name + " moderate all free parameters by default?\n\nOK: moderate all parameters\nCancel: moderate none");
        data.tipred_apply_default = true;
      }
      editor.cy.add({ group: "nodes", classes: kind, data: data, position: semanticPosition(editor, kind, index, kind + ":" + name) });
      if (kind === "manifest" && measuring) {
        addEdge(editor, latent.id(), data.id);
      } else send(editor, true);
    };
    if (["latent", "manifest", "tdpred", "tipred"].indexOf(kind) >= 0) {
      visualVariableDialog(editor, kind, prefix, create);
    }
  }

  function updateDataChoices(editor, columns, roles) {
    editor.dataColumns = columns || [];
    editor.dataRoles = roles || {};
  }

  function updateTipredActions(editor) {
    var actions = editor.tools.querySelector("[data-tipred-actions]");
    if (!actions) return;
    var selected = editor.cy.$("node.tipred:selected");
    actions.style.display = editor.view === "tipred_effects" && selected.length ? "inline" : "none";
  }

  function updateRandomEffectAction(editor) {
    var action = editor.tools.querySelector("[data-random-effect-action]");
    if (!action) return;
    var selected = editor.cy.$("node.parameter:selected").first();
    var available = editor.view === "tipred_effects" && selected.length && !selected.data("expression");
    action.style.display = available ? "inline" : "none";
    if (available) action.textContent = selected.data("random_effect") ? "Disable random effect" : "Enable random effect";
  }

  function toggleRandomEffect(editor) {
    var selected = editor.cy.$("node.parameter:selected").first();
    if (!selected.length || selected.data("expression")) return;
    Shiny.setInputValue(editor.id + "_toggle_random_effect", {
      view: editor.view,
      matrix: selected.data("matrix"),
      row: selected.data("row"),
      col: selected.data("col")
    }, { priority: "event" });
  }

  function applyTipredDefault(editor, moderateAll) {
    var tipredNode = editor.cy.$("node.tipred:selected").first();
    if (!tipredNode.length) return;
    var tipred = tipredNode.data("name");
    tipredNode.data("tipred_default", moderateAll);
    tipredNode.data("tipred_apply_default", true);
    editor.cy.edges().filter(function (edge) {
      return edge.data("edge_kind") === "tipred_effect" && edge.data("tipred") === tipred;
    }).remove();
    if (moderateAll) editor.cy.nodes("node.parameter").forEach(function (node) {
      var row = node.data("row"), col = node.data("col"), matrix = node.data("matrix");
      var id = ["tipred_effect", tipred, matrix, row, col].join("\r");
      editor.cy.add({ group: "edges", data: { id: id, matrix: matrix, row: row, col: col, source: tipredNode.id(), target: node.id(), directed: true, edge_kind: "tipred_effect", tipred: tipred, colour: tipredNode.data("colour"), value: "1", label: "" }, classes: cssClass({ directed: true, edge_kind: "tipred_effect", fixed: false }) });
    });
    send(editor, true);
  }

  function renderLegend(editor, graph) {
    var pathItems = [
      ["path_mark", "Free path"], ["path_mark fixed", "Fixed path"],
      ["path_mark random", "Random effect (red underlay)"], ["path_mark nonlinear", "Nonlinear expression"],
      ["path_mark variance_correlation", "Variance / correlation"], ["path_mark noise_input", "Noise input"]
    ];
    var items;
    if (editor.view === "initial_state") {
      items = [["node latent", "Latent process"], ["node constant", "Constant"],
        ["node initial_noise", "Initial-state noise (T0VAR)"],
        ["path_mark inactive", "T0VAR ignored (random T0MEANS)"]].concat(pathItems);
    } else if (editor.view === "tipred_effects") {
      items = [["node tipred", "TI predictor"], ["node parameter", "Free parameter"],
        ["path_mark", "TI predictor effect"], ["path_mark variance_correlation", "Random-effect variance / correlation"]];
    } else {
      items = [["node latent", "Latent process"], ["node manifest", "Observed manifest"],
        ["node tdpred", "Time-dependent predictor"], ["node tipred", "TI predictor"],
        ["node constant", "Constant"],
        ["node system_noise", "System / measurement noise"]].concat(pathItems);
    }
    clearElement(editor.legend);
    editor.legend.appendChild(makeElement("h5", "", "Legend"));
    items.forEach(function (item) {
      var row = makeElement("div", "ctgui-legend-item");
      row.appendChild(makeElement("span", "ctgui-legend-mark " + item[0]));
      row.appendChild(document.createTextNode(item[1]));
      editor.legend.appendChild(row);
    });
    var tipreds = (graph.nodes || []).filter(function (node) { return node.kind === "tipred"; });
    if (tipreds.length) {
      editor.legend.appendChild(makeElement("h6", "", "TI effect colours"));
      tipreds.forEach(function (node) {
        var row = makeElement("div", "ctgui-legend-item");
        var line = makeElement("span", "ctgui-legend-line");
        line.style.borderColor = isSafeColour(node.colour) ? node.colour : "#0f766e";
        row.appendChild(line);
        row.appendChild(document.createTextNode(String(node.name || "")));
        editor.legend.appendChild(row);
      });
    }
  }

  function updateTiFilters(editor, graph) {
    clearElement(editor.filters);
    if (editor.view !== "tipred_effects") return;
    var addToggle = function (host, field, text, checked) {
      var label = makeElement("label");
      var input = makeElement("input");
      input.type = "checkbox"; input.dataset[field] = "true"; input.checked = checked;
      label.appendChild(input); label.appendChild(document.createTextNode(" " + text));
      host.appendChild(label);
    };
    var structuralFilters = makeElement("div", "ctgui-filter-group ctgui-structural-filters");
    editor.filters.appendChild(structuralFilters);
    addToggle(structuralFilters, "showTipreds", "Show TI predictors", true);
    addToggle(structuralFilters, "showRandomEffects", "Show random-effects variance / correlation", true);
    var matrices = (graph.matrices || []).filter(function (matrix) { return matrix; });
    var matrixFilters = makeElement("div", "ctgui-filter-group ctgui-matrix-filters");
    editor.filters.appendChild(matrixFilters);
    matrixFilters.appendChild(makeElement("span", "", "Show parameter matrices:"));
    matrices.forEach(function (matrix) {
      var label = makeElement("label");
      var input = makeElement("input");
      input.type = "checkbox"; input.dataset.filter = String(matrix); input.checked = true;
      label.appendChild(input);
      label.appendChild(document.createTextNode(" " + String(matrix)));
      matrixFilters.appendChild(label);
    });
    var apply = function () {
      var visible = {};
      editor.filters.querySelectorAll("input[data-filter]").forEach(function (input) { visible[input.getAttribute("data-filter")] = input.checked; });
      var showTipreds = editor.filters.querySelector("input[data-show-tipreds]").checked;
      var showRandomEffects = editor.filters.querySelector("input[data-show-random-effects]").checked;
      editor.cy.nodes("node.parameter").forEach(function (node) { node.style("display", visible[node.data("matrix")] ? "element" : "none"); });
      editor.cy.edges("edge.tipred-effect").forEach(function (edge) { edge.style("display", visible[edge.data("matrix")] ? "element" : "none"); });
      editor.cy.nodes("node.tipred").style("display", showTipreds ? "element" : "none");
      editor.cy.edges("edge.tipred-effect").forEach(function (edge) {
        if (!showTipreds) edge.style("display", "none");
      });
      editor.cy.edges("edge.random_effect_variance, edge.random_effect_correlation").style("display", showRandomEffects ? "element" : "none");
    };
    editor.filters.onchange = apply; apply();
  }

  function resetLayout(editor) {
    var counters = { latent: 0, manifest: 0, tdpred: 0, system_noise: 0, measurement_noise: 0, initial_noise: 0 };
    editor.cy.nodes().forEach(function (node) {
      var kind = node.data("kind"), index = counters[kind] || 0;
      counters[kind] = index + 1;
      if (editor.view === "tipred_effects" || kind === "parameter" || kind === "tipred") return;
      node.position(semanticPosition(editor, kind, index, node.id()));
    });
    send(editor, false, true);
  }

  function renameSelectedVariable(editor) {
    var node = editor.cy.$("node:selected")[0];
    if (!node || ["latent", "manifest", "tdpred", "tipred"].indexOf(node.data("kind")) < 0) {
      window.alert("Select a latent, manifest, time-dependent predictor, or TI predictor first."); return;
    }
    var name = window.prompt("Variable name", node.data("name"));
    if (!name) return;
    name = name.trim();
    if (!/^[A-Za-z._][A-Za-z0-9._]*$/.test(name)) { window.alert("Use a valid R variable name."); return; }
    if (editor.cy.nodes().some(function (other) { return other !== node && other.data("name") === name; })) { window.alert("Names must be unique."); return; }
    var oldName = node.data("name");
    node.data("name", name); node.data("label", name);
    // Row and column names carry ctsem's row-target/column-source identity;
    // update those explicit identities while keeping Cytoscape element IDs stable.
    editor.cy.edges().forEach(function (edge) {
      if (edge.data("row") === oldName) edge.data("row", name);
      if (edge.data("col") === oldName) edge.data("col", name);
    });
    send(editor, true);
  }

  function removeSelection(editor) {
    var selected = editor.cy.$(":selected");
    if (!selected.length) return;
    var removable = selected.filter(function (element) {
      return !(element.isNode() && element.data("kind") === "parameter");
    });
    if (!removable.length) { warn(editor, "This selection is visual-only and cannot be deleted."); return; }
    removable.remove(); send(editor, true); select(editor, null);
  }

  function inferredMatrix(sourceNode, targetNode) {
    var source = sourceNode.data("kind"), target = targetNode.data("kind");
    if (source === "latent" && target === "latent") return "DRIFT";
    if (source === "latent" && target === "manifest") return "LAMBDA";
    if (source === "tdpred" && target === "latent") return "TDPREDEFFECT";
    if (sourceNode.id() === "constant:CINT" && target === "latent") return "CINT";
    if (sourceNode.id() === "constant:T0MEANS" && target === "latent") return "T0MEANS";
    if (sourceNode.id() === "constant:MANIFESTMEANS" && target === "manifest") return "MANIFESTMEANS";
    if (source === "system_noise" && (target === "latent" || target === "system_noise")) return "DIFFUSION";
    if (source === "measurement_noise" && (target === "manifest" || target === "measurement_noise")) return "MANIFESTVAR";
    if (source === "initial_noise" && (target === "latent" || target === "initial_noise")) return "T0VAR";
    if (source === "tipred" && target === "parameter") return "TIpredEffect";
    return null;
  }

  function addEdge(editor, source, target) {
    var sourceNode = editor.cy.getElementById(source), targetNode = editor.cy.getElementById(target);
    if (!sourceNode.length || !targetNode.length) return;
    var matrix = inferredMatrix(sourceNode, targetNode);
    if (!matrix) { warn(editor, "Those node types cannot be connected."); return; }
    var row, col, directed = true, edgeKind = "path";
    var sourceName = sourceNode.data("name"), targetName = targetNode.data("name");
    if (matrix === "DRIFT") { row = targetName; col = sourceName; }
    else if (matrix === "LAMBDA") { row = targetName; col = sourceName; }
    else if (matrix === "TDPREDEFFECT") { row = targetName; col = sourceName; }
    else if (matrix === "CINT" || matrix === "MANIFESTMEANS" || matrix === "T0MEANS") { row = targetName; col = sourceNode.data("matrix_col") || matrix; }
    else if (matrix === "TIpredEffect") {
      row = targetNode.data("row"); col = targetNode.data("col");
      var tipred = sourceName;
      var tipredId = ["tipred_effect", tipred, targetNode.data("matrix"), row, col].join("\r");
      if (editor.cy.getElementById(tipredId).length) return;
      editor.cy.add({ group: "edges", data: { id: tipredId, matrix: targetNode.data("matrix"), row: row, col: col, source: source, target: target, directed: true, edge_kind: "tipred_effect", tipred: tipred, colour: sourceNode.data("colour"), value: "1", label: "" }, classes: cssClass({ directed: true, edge_kind: "tipred_effect", fixed: false }) });
      send(editor, true); return;
    } else if (matrix === "DIFFUSION" || matrix === "MANIFESTVAR" || matrix === "T0VAR") {
      var sourceKind = sourceNode.data("kind"), targetKind = targetNode.data("kind");
      var noiseName = function (node) { var pieces = node.id().split(":"); return pieces[pieces.length - 1]; };
      if (source === target) {
        directed = false; edgeKind = "variance";
        row = noiseName(sourceNode); col = row;
      } else if ((sourceKind === "system_noise" || sourceKind === "measurement_noise" || sourceKind === "initial_noise") && (targetKind === "latent" || targetKind === "manifest")) {
        return;
      } else {
        directed = false; edgeKind = "correlation";
        row = noiseName(sourceNode); col = noiseName(targetNode);
        if (row < col) { var swap = row; row = col; col = swap; }
      }
    } else return;
    var value = "__free__";
    if (matrix === "LAMBDA") {
      var hasNumericLoading = editor.cy.edges().some(function (edge) {
        if (edge.data("matrix") !== "LAMBDA" || edge.data("col") !== sourceName) return false;
        var base = String(edge.data("value") || "").split("|")[0];
        return base !== "" && !isNaN(Number(base));
      });
      if (!hasNumericLoading) value = "1";
    }
    var id = [matrix, row, col].join("\r");
    if (editor.cy.getElementById(id).length) return;
    editor.cy.add({ group: "edges", data: { id: id, matrix: matrix, row: row, col: col, source: source, target: target, directed: directed, edge_kind: edgeKind, value: value, label: value === "__free__" ? "free" : value }, classes: cssClass({ directed: directed, edge_kind: edgeKind, fixed: value !== "__free__" }) });
    send(editor, true);
  }

  function warn(editor, message) {
    if (!editor.warning) return;
    editor.warning.textContent = message;
    editor.warning.classList.add("is-visible");
    window.clearTimeout(editor.warningTimer);
    editor.warningTimer = window.setTimeout(function () {
      editor.warning.classList.remove("is-visible");
    }, 2800);
  }

  function pointerModelPosition(editor, event) {
    var rect = editor.canvas.getBoundingClientRect();
    var pan = editor.cy.pan(), zoom = editor.cy.zoom();
    return { x: (event.clientX - rect.left - pan.x) / zoom, y: (event.clientY - rect.top - pan.y) / zoom };
  }

  function showRightDragPreview(editor, event) {
    var position = pointerModelPosition(editor, event);
    editor.rightDragPreviewNode = "__ctgui_path_preview_node__";
    editor.rightDragPreviewEdge = "__ctgui_path_preview_edge__";
    editor.cy.remove("#" + editor.rightDragPreviewNode + ", #" + editor.rightDragPreviewEdge);
    editor.cy.add([
      { group: "nodes", data: { id: editor.rightDragPreviewNode, preview: true, label: "" }, position: position, classes: "path-preview-target", selectable: false, grabbable: false },
      { group: "edges", data: { id: editor.rightDragPreviewEdge, source: editor.rightDragSource.id(), target: editor.rightDragPreviewNode, preview: true, directed: true, label: "" }, classes: "path-preview", selectable: false }
    ]);
  }

  function moveRightDragPreview(editor, event) {
    if (!editor.rightDragPreviewNode) return;
    editor.cy.getElementById(editor.rightDragPreviewNode).position(pointerModelPosition(editor, event));
  }

  function clearRightDragPreview(editor) {
    if (editor.rightDragPreviewNode) editor.cy.remove("#" + editor.rightDragPreviewNode);
    editor.rightDragPreviewNode = null; editor.rightDragPreviewEdge = null;
  }

  function addButton(parent, text, attribute, value) {
    var button = makeElement("button", "", text);
    button.type = "button";
    button.setAttribute(attribute, value);
    parent.appendChild(button);
    return button;
  }

  function addStructureGroup(tools, views) {
    var group = makeElement("span", "ctgui-structure-tools");
    group.dataset.views = views;
    tools.appendChild(group);
    return group;
  }

  function buildTools(tools) {
    var group = addStructureGroup(tools, "state_space");
    addButton(group, "Add latent", "data-add", "latent");

    group = addStructureGroup(tools, "state_space");
    addButton(group, "Add manifest", "data-add", "manifest");
    addButton(group, "Add time-dependent predictor", "data-add", "tdpred");

    group = addStructureGroup(tools, "state_space,tipred_effects");
    var randomEffectAction = addButton(group, "Enable random effect", "data-action", "toggle-random-effect");
    randomEffectAction.dataset.randomEffectAction = "";
    randomEffectAction.style.display = "none";
    addButton(group, "Add time-independent predictor", "data-add", "tipred");
    addButton(group, "Rename selected", "data-action", "rename");

    group = makeElement("span");
    group.dataset.tipredActions = "";
    group.style.display = "none";
    addButton(group, "Moderate all", "data-action", "tipred-all");
    addButton(group, "Moderate none", "data-action", "tipred-none");
    tools.appendChild(group);

    addButton(tools, "Delete selection", "data-action", "delete");
    addButton(tools, "Mode: move nodes", "data-action", "mode");
    addButton(tools, "Reset layout", "data-action", "fit");
  }

  function init(el) {
    if (editors[el.id]) return editors[el.id];
    var shell = document.createElement("div"); shell.className = "ctgui-visual-shell"; shell.tabIndex = 0;
    var tools = document.createElement("div"); tools.className = "ctgui-visual-tools";
    buildTools(tools);
    var canvas = document.createElement("div"); canvas.className = "ctgui-visual-canvas";
    canvas.tabIndex = 0;
    var filters = document.createElement("div"); filters.className = "ctgui-visual-filters";
    var body = document.createElement("div"); body.className = "ctgui-visual-body";
    var legend = document.createElement("aside"); legend.className = "ctgui-visual-legend";
    var warning = makeElement("div", "ctgui-visual-warning");
    warning.setAttribute("role", "status"); body.appendChild(canvas); body.appendChild(legend); shell.appendChild(tools); shell.appendChild(filters); shell.appendChild(body); shell.appendChild(warning); el.appendChild(shell);
    var editor = { id: el.id, el: el, shell: shell, canvas: canvas, tools: tools, filters: filters, legend: legend, warning: warning, warningTimer: null, dataColumns: [], dataRoles: {}, view: "state_space", cy: null, rightDragSource: null, rightDragTarget: null, rightDragSourcePosition: null, rightDragSourceWasGrabbable: false, rightDragPreviewNode: null, rightDragPreviewEdge: null, drawSource: null, drawTarget: null, drawMoved: false, pendingSource: null, suppressTap: false, mode: "move" };
    editor.cy = cytoscape({ container: canvas, elements: [], boxSelectionEnabled: true,
      style: [
        { selector: "node", style: { label: "data(label)", "text-wrap": "wrap", "text-valign": "center", "text-halign": "center", "background-color": "#f8fafc", "border-color": "#475569", "border-width": 1.5, width: 72, height: 48, "font-size": 12 } },
        { selector: "node.latent", style: { shape: "ellipse", "background-color": "#dbeafe", "border-color": "#1d4ed8" } },
        { selector: "node.manifest", style: { shape: "round-rectangle" } },
        { selector: "node.tdpred", style: { shape: "diamond", "background-color": "#dcfce7", "border-color": "#15803d" } },
        { selector: "node.tipred", style: { shape: "round-rectangle", "background-color": "data(colour)", "border-color": "#334155" } },
        { selector: "node.parameter", style: { shape: "ellipse", "background-color": "#fef3c7", "border-color": "#b45309", width: 105, height: 58, "font-size": 9 } },
        { selector: "node.constant", style: { shape: "triangle", width: 38, height: 38, "background-color": "#fef3c7", "border-color": "#b45309" } },
        { selector: "node.system_noise, node.measurement_noise, node.initial_noise", style: { shape: "ellipse", width: 46, height: 46, "font-size": 9, "background-color": "#f3e8ff", "border-color": "#7e22ce" } },
        { selector: "node.path-preview-target", style: { width: 3, height: 3, opacity: 0, events: "no" } },
        { selector: "edge", style: { label: "data(label)", "font-size": 10, width: 2.5, "line-color": "#2563eb", "target-arrow-color": "#2563eb", "target-arrow-shape": "triangle", "curve-style": "bezier", "text-background-color": "#fff", "text-background-opacity": 0.9, "text-background-padding": 2 } },
        { selector: "edge.undirected", style: { "target-arrow-shape": "none", "source-arrow-shape": "none", "line-style": "dotted", "line-color": "#7c3aed" } },
        { selector: "edge.fixed", style: { "line-color": "#94a3b8", "target-arrow-color": "#94a3b8", "line-style": "dashed" } },
        { selector: "edge.noise_input", style: { "line-color": "#111827", "target-arrow-color": "#111827", "target-arrow-shape": "triangle", "source-arrow-shape": "none", width: 1.5, "line-style": "solid" } },
        { selector: "edge.custom", style: { "line-color": "#7c3aed", "target-arrow-color": "#7c3aed" } },
        { selector: "edge.moderated", style: { "line-style": "solid" } },
        { selector: "edge.variance", style: { "curve-style": "bezier", "loop-direction": "-90deg", "loop-sweep": "25deg", "line-style": "solid", "line-color": "#111827", "source-arrow-color": "#111827", "target-arrow-color": "#111827", "source-arrow-shape": "triangle", "target-arrow-shape": "triangle", width: 2.5 } },
        { selector: "edge.correlation", style: { "curve-style": "unbundled-bezier", "control-point-distances": "30px", "control-point-weights": "0.5", "line-style": "solid", "line-color": "#111827", "source-arrow-color": "#111827", "target-arrow-color": "#111827", "source-arrow-shape": "triangle", "target-arrow-shape": "triangle", width: 2.5 } },
        { selector: "edge.inactive", style: { "line-color": "#9ca3af", "source-arrow-color": "#9ca3af", "target-arrow-color": "#9ca3af", "line-style": "dotted", "text-opacity": 0.8 } },
        { selector: "edge.nonlinear", style: { "line-color": "#7c3aed", "target-arrow-color": "#7c3aed", "source-arrow-color": "#7c3aed", "curve-style": "round-segments", "segment-weights": "0.16 0.32 0.48 0.64 0.80", "segment-distances": "12 -12 12 -12 12", "line-style": "solid" } },
        { selector: "edge.random", style: { "underlay-color": "#dc2626", "underlay-opacity": 0.85, "underlay-padding": 3 } },
        { selector: "edge.tipred-effect", style: { "line-color": "data(colour)", "target-arrow-color": "data(colour)", width: 3.2 } },
        { selector: "edge.random_effect_variance, edge.random_effect_correlation", style: { "line-style": "solid", "line-color": "#111827", "source-arrow-color": "#111827", "target-arrow-color": "#111827", "source-arrow-shape": "triangle", "target-arrow-shape": "triangle", width: 2.2 } },
        { selector: "edge.random_effect_variance", style: { "loop-direction": "-90deg", "loop-sweep": "270deg", "control-point-step-size": 120 } },
        { selector: "edge.path-preview", style: { "line-style": "dashed", "line-color": "#16a34a", "target-arrow-color": "#16a34a", "target-arrow-shape": "triangle", width: 2.5, opacity: 0.85 } },
        { selector: ":selected", style: { "border-width": 4, "border-color": "#f59e0b", "line-color": "#f59e0b", "target-arrow-color": "#f59e0b" } },
        { selector: "node.path-source", style: { "border-width": 4, "border-color": "#16a34a" } }
      ]
    });
    editor.cy.on("select", "edge", function (event) { select(editor, event.target); });
    editor.cy.on("select", "node.parameter", function (event) { select(editor, event.target); updateRandomEffectAction(editor); });
    editor.cy.on("select", "node.tipred", function (event) { select(editor, event.target); updateTipredActions(editor); });
    editor.cy.on("unselect", "node.parameter", function () { window.setTimeout(function () { updateRandomEffectAction(editor); }, 0); });
    editor.cy.on("unselect", "edge", function () { window.setTimeout(function () { if (!editor.cy.$("edge:selected").length) select(editor, null); }, 0); });
    editor.cy.on("unselect", "node.tipred", function () { window.setTimeout(function () { updateTipredActions(editor); }, 0); });
    editor.cy.on("dragfree", "node", function () { send(editor, false, true); });
    canvas.addEventListener("contextmenu", function (event) { event.preventDefault(); });
    canvas.addEventListener("pointerdown", function () { shell.focus(); }, true);
    var nodeAtPointer = function (event) {
      var rect = canvas.getBoundingClientRect(), x = event.clientX - rect.left, y = event.clientY - rect.top, found = null;
      editor.cy.nodes().forEach(function (node) {
        if (node.data("preview")) return;
        var box = node.renderedBoundingBox({ includeLabels: false, includeOverlays: false });
        if (x >= box.x1 && x <= box.x2 && y >= box.y1 && y <= box.y2) found = node;
      });
      return found;
    };
    canvas.addEventListener("pointerdown", function (event) {
      if (event.button !== 2) return;
      var source = nodeAtPointer(event);
      if (!source) return;
      editor.rightDragSource = source; editor.rightDragTarget = null;
      editor.rightDragSourcePosition = Object.assign({}, source.position());
      editor.rightDragSourceWasGrabbable = source.grabbable(); source.ungrabify();
      source.addClass("path-source");
      showRightDragPreview(editor, event);
      if (event.target.setPointerCapture) event.target.setPointerCapture(event.pointerId);
      event.preventDefault(); event.stopImmediatePropagation();
    }, true);
    canvas.addEventListener("pointermove", function (event) {
      if (!editor.rightDragSource || !(event.buttons & 2)) return;
      editor.rightDragTarget = nodeAtPointer(event);
      moveRightDragPreview(editor, event);
      event.preventDefault(); event.stopImmediatePropagation();
    }, true);
    canvas.addEventListener("pointerup", function (event) {
      if (event.button !== 2 || !editor.rightDragSource) return;
      var source = editor.rightDragSource, target = nodeAtPointer(event) || editor.rightDragTarget;
      var sourceId = source.id(), targetId = target ? target.id() : null;
      var sourcePosition = editor.rightDragSourcePosition ? Object.assign({}, editor.rightDragSourcePosition) : null;
      clearRightDragPreview(editor);
      if (sourcePosition) source.position(sourcePosition);
      if (editor.mode === "move") source.grabify();
      source.removeClass("path-source"); editor.rightDragSource = null; editor.rightDragTarget = null;
      editor.rightDragSourcePosition = null; editor.rightDragSourceWasGrabbable = false;
      if (event.target.releasePointerCapture && event.target.hasPointerCapture && event.target.hasPointerCapture(event.pointerId)) event.target.releasePointerCapture(event.pointerId);
      event.preventDefault(); event.stopImmediatePropagation();
      window.setTimeout(function () {
        var latestSource = editor.cy.getElementById(sourceId);
        if (sourcePosition && latestSource.length) latestSource.position(sourcePosition);
        if (targetId) addEdge(editor, sourceId, targetId);
      }, 0);
    }, true);
    canvas.addEventListener("pointercancel", function () {
      if (!editor.rightDragSource) return;
      clearRightDragPreview(editor);
      if (editor.rightDragSourcePosition) editor.rightDragSource.position(editor.rightDragSourcePosition);
      if (editor.mode === "move") editor.rightDragSource.grabify();
      editor.rightDragSource.removeClass("path-source"); editor.rightDragSource = null; editor.rightDragTarget = null;
      editor.rightDragSourcePosition = null; editor.rightDragSourceWasGrabbable = false;
    }, true);
    editor.cy.on("tapstart", "node", function (event) {
      if (editor.mode === "draw") { editor.drawSource = event.target; editor.drawTarget = null; editor.drawMoved = false; }
    });
    editor.cy.on("tapdragover", "node", function (event) {
      if (editor.mode === "draw") { editor.drawMoved = true; editor.drawTarget = event.target; }
    });
    editor.cy.on("tapend", function (event) {
      if (editor.mode !== "draw" || !editor.drawSource) return;
      var source = editor.drawSource, target = editor.drawTarget;
      editor.drawSource = null; editor.drawTarget = null;
      if (editor.drawMoved && target && target.isNode && target.isNode()) {
        editor.suppressTap = true;
        window.setTimeout(function () { editor.suppressTap = false; }, 0);
        addEdge(editor, source.id(), target.id());
      }
      editor.drawMoved = false;
    });
    editor.cy.on("tap", "node", function (event) {
      if (editor.mode !== "draw" || editor.suppressTap) return;
      if (!editor.pendingSource || !editor.pendingSource.length) {
        editor.pendingSource = event.target;
        editor.pendingSource.addClass("path-source");
        return;
      }
      var source = editor.pendingSource;
      source.removeClass("path-source");
      editor.pendingSource = null;
      addEdge(editor, source.id(), event.target.id());
    });
    editor.keyboardActive = false;
    var deleteSelectionOnKey = function (event) {
      var tag = (event.target && event.target.tagName || "").toLowerCase();
      if (!editor.keyboardActive || (event.key !== "Delete" && event.key !== "Backspace") || tag === "input" || tag === "textarea" || tag === "select" || event.target.isContentEditable) return;
      var selected = editor.cy.$(":selected");
      if (selected.length) { event.preventDefault(); event.stopImmediatePropagation(); removeSelection(editor); }
    };
    shell.addEventListener("keydown", deleteSelectionOnKey, true);
    window.addEventListener("keydown", deleteSelectionOnKey, true);
    canvas.addEventListener("pointerdown", function () { editor.keyboardActive = true; }, true);
    editor.cy.on("select", function () { editor.keyboardActive = true; });
    tools.addEventListener("click", function (event) {
      var add = event.target.getAttribute("data-add"), action = event.target.getAttribute("data-action");
      if (add) addVariable(editor, add, add === "latent" ? "eta" : add === "manifest" ? "y" : add === "tdpred" ? "x" : "z");
      if (action === "rename") renameSelectedVariable(editor);
      if (action === "toggle-random-effect") toggleRandomEffect(editor);
      if (action === "tipred-all") applyTipredDefault(editor, true);
      if (action === "tipred-none") applyTipredDefault(editor, false);
      if (action === "delete") removeSelection(editor);
      if (action === "fit") resetLayout(editor);
      if (action === "mode") {
        editor.mode = editor.mode === "move" ? "draw" : "move";
        event.target.textContent = editor.mode === "draw" ? "Mode: draw paths" : "Mode: move nodes";
        if (editor.pendingSource && editor.pendingSource.length) editor.pendingSource.removeClass("path-source");
        editor.pendingSource = null;
        if (editor.mode === "draw") editor.cy.nodes().ungrabify(); else editor.cy.nodes().grabify();
      }
    });
    editors[el.id] = editor; return editor;
  }

  function load(message) {
    if (!message || typeof message.id !== "string" || !validGraph(message.graph)) {
      window.console.warn("ctsemGUI ignored an unsupported visual graph message.");
      return;
    }
    var target = document.getElementById(message.id);
    if (!target) {
      window.console.warn("ctsemGUI could not find the visual graph target.");
      return;
    }
    var editor = init(target);
    var graph = message.graph; editor.view = graph.view || "state_space";
    editor.pendingSource = null; editor.drawSource = null; editor.drawTarget = null;
    editor.cy.elements().remove();
    editor.cy.add((graph.nodes || []).map(function (node) { return { group: "nodes", data: node, position: { x: node.x, y: node.y }, classes: node.kind }; }));
    editor.cy.add((graph.edges || []).map(function (edge) { return { group: "edges", data: edge, classes: cssClass(edge), selectable: edge.selectable !== false }; }));
    editor.tools.querySelectorAll("[data-views]").forEach(function (element) {
      var views = (element.getAttribute("data-views") || "").split(",");
      element.style.display = views.indexOf(editor.view) >= 0 ? "inline" : "none";
    });
    if (editor.mode === "draw") editor.cy.nodes().ungrabify(); else editor.cy.nodes().grabify();
    updateDataChoices(editor, message.data_columns, message.data_roles);
    updateTiFilters(editor, graph); renderLegend(editor, graph);
    updateTipredActions(editor);
    updateRandomEffectAction(editor);
    editor.cy.resize(); editor.cy.fit(undefined, 35);
  }

  Shiny.addCustomMessageHandler("ctgui-visual-load", load);
  Shiny.addCustomMessageHandler("ctgui-visual-update-edge", function (message) {
    window.ctguiVisualUpdateEdge(message.id, message.edge);
  });
  window.ctguiVisualUpdateEdge = function (id, edge) {
    var editor = editors[id]; if (!editor) return;
    var key = [edge.matrix, edge.row, edge.col].join("\r"), element = editor.cy.getElementById(key);
    if (!element.length) return;
    Object.keys(edge).forEach(function (name) { if (name !== "id" && name !== "source" && name !== "target") element.data(name, edge[name]); });
    element.data("label", edge.value || "0"); element.classes(cssClass(Object.assign({}, element.data())));
  };
})();
