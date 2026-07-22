/* ctsemgui visual specification editor.
 * Uses Cytoscape.js (MIT). Interaction design is independently implemented
 * for ctsemgui, with acknowledgement to lavaangui for the general idea of a
 * browser-native SEM diagram editor (GPL-3.0-or-later).
 */
(function () {
  var editors = {};

  function cssClass(edge) {
    if (edge.visual_only) return "directed noise_input";
    var classes = [edge.directed ? "directed" : "undirected", edge.edge_kind || "path"];
    classes.push(edge.fixed ? "fixed" : "free");
    if (edge.custom) classes.push("custom");
    if (edge.indvarying) classes.push("random");
    if (edge.tipred_effects && edge.tipred_effects.length) classes.push("moderated");
    return classes.join(" ");
  }

  function serialise(editor) {
    var cy = editor.cy;
    return {
      view: editor.view,
      nodes: cy.nodes().map(function (node) {
        var data = Object.assign({}, node.data());
        var position = node.position(); data.x = position.x; data.y = position.y;
        return data;
      }),
      edges: cy.edges().map(function (edge) { return Object.assign({}, edge.data()); })
    };
  }

  function send(editor, changed, layoutOnly) {
    var graph = serialise(editor);
    graph.changed = !!changed; graph.layout_only = !!layoutOnly; graph.nonce = Math.random();
    Shiny.setInputValue(editor.id + "_graph", graph, { priority: "event" });
  }

  function select(editor, element) {
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

  function addVariable(editor, kind, prefix) {
    var name = (kind === "manifest" || kind === "tdpred") && editor.dataChoice.value ? editor.dataChoice.value : window.prompt("Variable name", uniqueName(editor, prefix));
    if (!name) return;
    name = name.trim();
    if (!/^[A-Za-z._][A-Za-z0-9._]*$/.test(name)) { window.alert("Use a valid R variable name."); return; }
    if (editor.cy.nodes().some(function (node) { return node.data("name") === name; })) { window.alert("Names must be unique."); return; }
    editor.cy.add({ group: "nodes", classes: kind, data: { id: kind + ":" + name, kind: kind, name: name, label: name, original_name: name }, position: { x: 180, y: 120 + editor.cy.nodes().length * 24 } });
    send(editor, true);
  }

  function updateDataChoices(editor, columns) {
    var used = editor.cy.nodes().map(function (node) { return node.data("name"); });
    var available = (columns || []).filter(function (name) { return used.indexOf(name) < 0; });
    editor.dataChoice.innerHTML = "<option value=''>Enter a new name…</option>" + available.map(function (name) { return "<option value='" + name + "'>" + name + "</option>"; }).join("");
  }

  function renameSelectedVariable(editor) {
    var node = editor.cy.$("node:selected")[0];
    if (!node || ["latent", "manifest", "tdpred"].indexOf(node.data("kind")) < 0) {
      window.alert("Select a latent, manifest, or time-dependent predictor first."); return;
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
    return null;
  }

  function addEdge(editor, source, target) {
    var sourceNode = editor.cy.getElementById(source), targetNode = editor.cy.getElementById(target);
    if (!sourceNode.length || !targetNode.length) return;
    var matrix = inferredMatrix(sourceNode, targetNode);
    if (!matrix) return;
    var row, col, directed = true, edgeKind = "path";
    var sourceName = sourceNode.data("name"), targetName = targetNode.data("name");
    if (matrix === "DRIFT") { row = targetName; col = sourceName; }
    else if (matrix === "LAMBDA") { row = targetName; col = sourceName; }
    else if (matrix === "TDPREDEFFECT") { row = targetName; col = sourceName; }
    else if (matrix === "CINT" || matrix === "MANIFESTMEANS" || matrix === "T0MEANS") { row = targetName; col = matrix; }
    else if (matrix === "DIFFUSION" || matrix === "MANIFESTVAR" || matrix === "T0VAR") {
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
    var id = [matrix, row, col].join("\r");
    if (editor.cy.getElementById(id).length) return;
    editor.cy.add({ group: "edges", data: { id: id, matrix: matrix, row: row, col: col, source: source, target: target, directed: directed, edge_kind: edgeKind, value: "__free__", label: "free" }, classes: cssClass({ directed: directed, edge_kind: edgeKind, fixed: false }) });
    send(editor, true);
  }

  function init(el) {
    if (editors[el.id]) return editors[el.id];
    var shell = document.createElement("div"); shell.className = "ctgui-visual-shell";
    var tools = document.createElement("div"); tools.className = "ctgui-visual-tools";
    tools.innerHTML = '<button type="button" data-add="latent">Add latent</button><label>Dataset variable <select class="ctgui-data-choice"></select></label><button type="button" data-add="manifest">Add manifest</button><button type="button" data-add="tdpred">Add TD predictor</button><button type="button" data-action="rename">Rename selected</button><button type="button" data-action="delete">Delete selection</button><button type="button" data-action="loop">Add DRIFT self-loop</button><button type="button" data-action="mode">Mode: move nodes</button><button type="button" data-action="fit">Auto-layout</button><span class="ctgui-visual-gesture">Right-drag always draws; Draw mode supports left-drag or click source then target</span>';
    var canvas = document.createElement("div"); canvas.className = "ctgui-visual-canvas";
    shell.appendChild(tools); shell.appendChild(canvas); el.appendChild(shell);
    var editor = { id: el.id, el: el, canvas: canvas, tools: tools, dataChoice: tools.querySelector(".ctgui-data-choice"), view: "state_space", cy: null, rightDragSource: null, rightDragTarget: null, rightDragSourcePosition: null, rightDragSourceWasGrabbable: false, drawSource: null, drawTarget: null, drawMoved: false, pendingSource: null, suppressTap: false, mode: "move" };
    editor.cy = cytoscape({ container: canvas, elements: [], boxSelectionEnabled: true,
      style: [
        { selector: "node", style: { label: "data(label)", "text-wrap": "wrap", "text-valign": "center", "text-halign": "center", "background-color": "#f8fafc", "border-color": "#475569", "border-width": 1.5, width: 72, height: 48, "font-size": 12 } },
        { selector: "node.latent", style: { shape: "ellipse", "background-color": "#dbeafe", "border-color": "#1d4ed8" } },
        { selector: "node.manifest", style: { shape: "round-rectangle" } },
        { selector: "node.tdpred", style: { shape: "diamond", "background-color": "#dcfce7", "border-color": "#15803d" } },
        { selector: "node.constant", style: { shape: "triangle", width: 38, height: 38, "background-color": "#fef3c7", "border-color": "#b45309" } },
        { selector: "node.system_noise, node.measurement_noise, node.initial_noise", style: { shape: "ellipse", width: 46, height: 46, "font-size": 9, "background-color": "#f3e8ff", "border-color": "#7e22ce" } },
        { selector: "edge", style: { label: "data(label)", "font-size": 10, width: 2.5, "line-color": "#2563eb", "target-arrow-color": "#2563eb", "target-arrow-shape": "triangle", "curve-style": "bezier", "text-background-color": "#fff", "text-background-opacity": 0.9, "text-background-padding": 2 } },
        { selector: "edge.undirected", style: { "target-arrow-shape": "none", "source-arrow-shape": "none", "line-style": "dotted", "line-color": "#7c3aed" } },
        { selector: "edge.fixed", style: { "line-color": "#94a3b8", "target-arrow-color": "#94a3b8", "line-style": "dashed" } },
        { selector: "edge.noise_input", style: { "line-color": "#111827", "target-arrow-color": "#111827", "target-arrow-shape": "triangle", "source-arrow-shape": "none", width: 1.5, "line-style": "solid" } },
        { selector: "edge.custom", style: { "line-color": "#7c3aed", "target-arrow-color": "#7c3aed" } },
        { selector: "edge.moderated", style: { "line-style": "dashed" } },
        { selector: "edge.variance", style: { "curve-style": "bezier", "loop-direction": "-45deg", "loop-sweep": "65deg", "line-style": "solid", "line-color": "#111827", "source-arrow-color": "#111827", "target-arrow-color": "#111827", "source-arrow-shape": "triangle", "target-arrow-shape": "triangle", width: 2.5 } },
        { selector: "edge.correlation", style: { "line-style": "solid", "line-color": "#111827", "source-arrow-color": "#111827", "target-arrow-color": "#111827", "source-arrow-shape": "triangle", "target-arrow-shape": "triangle", width: 2.5 } },
        { selector: "edge.random", style: { "line-style": "dashed", "line-color": "#ea580c", "target-arrow-color": "#ea580c", "source-arrow-color": "#ea580c", width: 3.5 } },
        { selector: ":selected", style: { "border-width": 4, "border-color": "#f59e0b", "line-color": "#f59e0b", "target-arrow-color": "#f59e0b" } },
        { selector: "node.path-source", style: { "border-width": 4, "border-color": "#16a34a" } }
      ]
    });
    editor.cy.on("select", "edge", function (event) { select(editor, event.target); });
    editor.cy.on("unselect", "edge", function () { window.setTimeout(function () { if (!editor.cy.$("edge:selected").length) select(editor, null); }, 0); });
    editor.cy.on("dragfree", "node", function () { send(editor, false, true); });
    canvas.addEventListener("contextmenu", function (event) { event.preventDefault(); });
    var nodeAtPointer = function (event) {
      var rect = canvas.getBoundingClientRect(), x = event.clientX - rect.left, y = event.clientY - rect.top, found = null;
      editor.cy.nodes().forEach(function (node) {
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
      if (event.target.setPointerCapture) event.target.setPointerCapture(event.pointerId);
      event.preventDefault(); event.stopImmediatePropagation();
    }, true);
    canvas.addEventListener("pointermove", function (event) {
      if (!editor.rightDragSource || !(event.buttons & 2)) return;
      editor.rightDragTarget = nodeAtPointer(event);
      event.preventDefault(); event.stopImmediatePropagation();
    }, true);
    canvas.addEventListener("pointerup", function (event) {
      if (event.button !== 2 || !editor.rightDragSource) return;
      var source = editor.rightDragSource, target = nodeAtPointer(event) || editor.rightDragTarget;
      var sourceId = source.id(), targetId = target ? target.id() : null;
      var sourcePosition = editor.rightDragSourcePosition ? Object.assign({}, editor.rightDragSourcePosition) : null;
      if (sourcePosition) source.position(sourcePosition);
      if (editor.rightDragSourceWasGrabbable && editor.mode === "move") source.grabify();
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
      if (editor.rightDragSourcePosition) editor.rightDragSource.position(editor.rightDragSourcePosition);
      if (editor.rightDragSourceWasGrabbable && editor.mode === "move") editor.rightDragSource.grabify();
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
    document.addEventListener("keydown", function (event) {
      var tag = (event.target && event.target.tagName || "").toLowerCase();
      if ((event.key !== "Delete" && event.key !== "Backspace") || tag === "input" || tag === "textarea" || tag === "select") return;
      var selected = editor.cy.$(":selected");
      if (selected.length) { event.preventDefault(); selected.remove(); send(editor, true); select(editor, null); }
    });
    tools.addEventListener("click", function (event) {
      var add = event.target.getAttribute("data-add"), action = event.target.getAttribute("data-action");
      if (add) addVariable(editor, add, add === "latent" ? "eta" : add === "manifest" ? "y" : "x");
      if (action === "rename") renameSelectedVariable(editor);
      if (action === "delete") { var selected = editor.cy.$(":selected"); if (selected.length && window.confirm("Remove selected visual element(s)?")) { selected.remove(); send(editor, true); select(editor, null); } }
      if (action === "fit") { editor.cy.layout({ name: "breadthfirst", directed: true, padding: 35 }).run(); send(editor, false, true); }
      if (action === "loop") { var node = editor.cy.$("node.latent:selected")[0]; if (node) addEdge(editor, node.id(), node.id()); }
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
    var editor = init(document.getElementById(message.id)); if (!editor) return;
    var graph = message.graph; editor.view = graph.view || "state_space";
    editor.pendingSource = null; editor.drawSource = null; editor.drawTarget = null;
    editor.cy.elements().remove();
    editor.cy.add((graph.nodes || []).map(function (node) { return { group: "nodes", data: node, position: { x: node.x, y: node.y }, classes: node.kind }; }));
    editor.cy.add((graph.edges || []).map(function (edge) { return { group: "edges", data: edge, classes: cssClass(edge), selectable: edge.selectable !== false }; }));
    if (editor.mode === "draw") editor.cy.nodes().ungrabify();
    updateDataChoices(editor, message.data_columns);
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
