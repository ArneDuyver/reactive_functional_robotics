// ===============================
// Mermaid + MQTT State Diagram
// ===============================

// Ensure container exists
function ensureMermaidContainer() {
  if (!document.getElementById("mermaid-container")) {
    var div = document.createElement("div");
    div.id = "mermaid-container";
    div.className = "fade-in";
    div.textContent = "Loading...";
    document.body.appendChild(div);
  }
}

// Utility: load script only once
function loadScriptOnce(id, src, callback) {
  if (document.getElementById(id)) {
    if (callback) callback();
    return;
  }
  var script = document.createElement("script");
  script.src = src;
  script.id = id;
  if (callback) script.onload = callback;
  document.head.appendChild(script);
}

// Initialize Mermaid + App
function loadMermaidApp() {
  mermaid.initialize({ startOnLoad: true });
  ensureMermaidContainer();
  startApp();
}

// Load Mermaid first, then MQTT, then run app
loadScriptOnce(
  "mermaid-script",
  "https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js",
  function () {
    loadScriptOnce("mqtt-script", "https://unpkg.com/mqtt/dist/mqtt.min.js", function () {
      loadMermaidApp();
    });
  }
);

// Add Mermaid CSS
if (!document.getElementById("mermaid-css")) {
  var link = document.createElement("link");
  link.rel = "stylesheet";
  link.href = "https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.css";
  link.id = "mermaid-css";
  document.head.appendChild(link);
}

// Custom highlight style
if (!document.getElementById("mermaid-style")) {
  var style = document.createElement("style");
  style.id = "mermaid-style";
  style.textContent = `
    #mermaid-container {
      transition: opacity 0.5s ease;
      opacity: 1;
      min-height: 200px;
    }
    #mermaid-container.fade-in { opacity: 1; }
    #mermaid-container:not(.fade-in) { opacity: 0; }

    /* Mermaid custom styling voor highlighten */
    .mermaid .highlight>rect {
      fill: #ffeb3b !important;
      stroke: #f57f17 !important;
      stroke-width: 3px;
    }
    /* Error state styling */
    .mermaid .errorState>rect {
      fill: #ffebee !important;
      stroke: #c62828 !important;
      stroke-width: 2px !important;
    }
    .mermaid .error>rect {
      fill: #ffcdd2 !important;
      stroke: #d32f2f !important;
      stroke-width: 3px !important;
    }
  `;
  document.head.appendChild(style);
}

// ===============================
// Main App Logic
// ===============================

function startApp() {
  const brokerUrl = "ws://localhost:9001";
  const topicHighlight = "fsm/toenv";

  let diagramData = {
    classes: ["Simple", "End", "Error"],
    associations: [["Simple", "End"]],
  };

  let activeClass = "Simple";

  // Connect to MQTT
  const client = mqtt.connect(brokerUrl);

  client.on("connect", () => {
    console.log("Verbonden met MQTT broker");
    client.subscribe(topicHighlight, (err) => {
      if (!err) console.log("[DEBUG] Geabonneerd op " + topicHighlight);
    });
  });

  client.on("message", (topic, message) => {
    try {
      const data = JSON.parse(message.toString());
      if (topic === topicHighlight) {
        // Extract state from OutputState structure: data.outputData.state
        const newActiveClass = data.outputData && data.outputData.state ? data.outputData.state : null;
        console.log("[DEBUG] Received state:", newActiveClass);
        
        // Validate state exists in our diagram and update
        if (newActiveClass && diagramData.classes.includes(newActiveClass)) {
          activeClass = newActiveClass;
          console.log("[DEBUG] Updated active state to:", activeClass);
        } else {
          console.log("[DEBUG] State not found in diagram classes:", newActiveClass);
          // If invalid state received, default to Error state
          activeClass = "Error";
        }
        updateDiagram();
      }
    } catch (err) {
      console.error("[DEBUG] Fout bij parsen van MQTT bericht:", err);
      // On parsing error, set to Error state
      activeClass = "Error";
      updateDiagram();
    }
  });

  // Render diagram
  function updateDiagram() {
    const container = document.getElementById("mermaid-container");
    if (!container) return;

    let diagram = "graph LR\n";
    diagramData.classes.forEach((cls) => {
      if (cls === activeClass) {
        // Highlight the active state
        if (cls === "Error") {
          diagram += `${cls}(["${cls}"]):::error\n`;
        } else {
          diagram += `${cls}(["${cls}"]):::highlight\n`;
        }
      } else {
        // Regular state display
        if (cls === "Error") {
          diagram += `${cls}([${cls}]):::errorState\n`;
        } else {
          diagram += `${cls}(${cls})\n`;
        }
      }
    });
    diagramData.associations.forEach(([c1, c2]) => {
      diagram += `${c1} --> ${c2}\n`;
    });

    container.classList.remove("fade-in");
    container.innerHTML = `<pre class="mermaid">${diagram}</pre>`;

    mermaid.init(undefined, container.querySelector(".mermaid"));

    setTimeout(() => {
      container.classList.add("fade-in");
    }, 10);
  }

  // Initial draw
  document.addEventListener("DOMContentLoaded", updateDiagram);
  updateDiagram();
}
