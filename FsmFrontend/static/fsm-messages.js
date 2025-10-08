// ===============================
// FSM Messages MQTT Display
// ===============================

// Utility: load script only once (reused from mermaid-mqtt.js)
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

// Initialize FSM Messages App
function initializeFsmMessages() {
  console.log("[FSM-Messages] Initializing...");
  
  // Check if MQTT is already loaded, if not load it
  if (typeof mqtt === 'undefined') {
    loadScriptOnce("mqtt-script-fsm", "https://unpkg.com/mqtt/dist/mqtt.min.js", function () {
      startFsmMessagesApp();
    });
  } else {
    startFsmMessagesApp();
  }
}

// Main FSM Messages App Logic
function startFsmMessagesApp() {
  console.log("[FSM-Messages] Starting app...");
  
  const brokerUrl = "ws://localhost:9001";
  const topicMessages = "fsm/toenv";
  
  let refreshRate = 20; // Default 20ms
  let lastMessage = null;
  let updateTimeout = null;
  let isUpdating = false;
  
  // Get DOM elements
  const refreshInput = document.getElementById("refresh-rate-input");
  const messagesTextarea = document.getElementById("fsm-messages-display");
  
  if (!refreshInput || !messagesTextarea) {
    console.error("[FSM-Messages] Required DOM elements not found");
    return;
  }
  
  // Load saved refresh rate from localStorage
  const savedRefreshRate = localStorage.getItem('fsm-refresh-rate');
  if (savedRefreshRate) {
    refreshRate = parseInt(savedRefreshRate, 10);
    refreshInput.value = refreshRate;
  }
  
  // Connect to MQTT
  const client = mqtt.connect(brokerUrl);
  
  client.on("connect", () => {
    console.log("[FSM-Messages] Connected to MQTT broker");
    client.subscribe(topicMessages, (err) => {
      if (!err) {
        console.log("[FSM-Messages] Subscribed to " + topicMessages);
      } else {
        console.error("[FSM-Messages] Failed to subscribe:", err);
      }
    });
  });
  
  client.on("error", (err) => {
    console.error("[FSM-Messages] MQTT connection error:", err);
    updateMessagesDisplay("ERROR: Failed to connect to MQTT broker");
  });
  
  client.on("message", (topic, message) => {
    if (topic === topicMessages) {
      try {
        const messageStr = message.toString();
        lastMessage = {
          timestamp: new Date(),
          data: JSON.parse(messageStr),
          raw: messageStr
        };
        
        // If not currently updating, schedule an update
        if (!isUpdating) {
          scheduleUpdate();
        }
      } catch (err) {
        console.error("[FSM-Messages] Error parsing MQTT message:", err);
        lastMessage = {
          timestamp: new Date(),
          data: null,
          raw: message.toString(),
          error: err.message
        };
        
        if (!isUpdating) {
          scheduleUpdate();
        }
      }
    }
  });
  
  // Schedule update based on refresh rate
  function scheduleUpdate() {
    if (updateTimeout) {
      clearTimeout(updateTimeout);
    }
    
    isUpdating = true;
    updateTimeout = setTimeout(() => {
      if (lastMessage) {
        updateMessagesDisplay(formatMessage(lastMessage));
      }
      isUpdating = false;
    }, refreshRate);
  }
  
  // Format message for display
  function formatMessage(messageObj) {
    const timestamp = messageObj.timestamp.toISOString();
    
    if (messageObj.error) {
      return `[${timestamp}] ERROR: ${messageObj.error}\nRaw message: ${messageObj.raw}`;
    }
    
    let formatted = `[${timestamp}]\n`;
    
    if (messageObj.data) {
      // Pretty print the JSON with proper formatting
      formatted += JSON.stringify(messageObj.data, null, 2);
    } else {
      formatted += messageObj.raw;
    }
    
    return formatted;
  }
  
  // Update the textarea display
  function updateMessagesDisplay(newContent) {
    if (!messagesTextarea) return;
    
    // Overwrite the previous content with new content
    messagesTextarea.value = newContent;
    
    // Auto-scroll to top to show the message
    messagesTextarea.scrollTop = 0;
  }
  
  // Handle refresh rate input changes
  refreshInput.addEventListener('input', function() {
    const newRate = parseInt(this.value, 10);
    if (!isNaN(newRate) && newRate > 0) {
      refreshRate = newRate;
      localStorage.setItem('fsm-refresh-rate', refreshRate.toString());
      console.log("[FSM-Messages] Refresh rate updated to:", refreshRate, "ms");
    }
  });
  
  // Handle refresh rate input blur (when user clicks away)
  refreshInput.addEventListener('blur', function() {
    const newRate = parseInt(this.value, 10);
    if (isNaN(newRate) || newRate <= 0) {
      // Reset to previous valid value
      this.value = refreshRate;
    }
  });
  
  // Clear button functionality
  const clearButton = document.getElementById("clear-messages-btn");
  if (clearButton) {
    clearButton.addEventListener('click', function() {
      messagesTextarea.value = "";
      console.log("[FSM-Messages] Messages cleared");
    });
  }
  
  console.log("[FSM-Messages] App started successfully");
}

// Auto-initialize when DOM is ready
document.addEventListener('DOMContentLoaded', function() {
  initializeFsmMessages();
});

// Also try to initialize immediately in case DOM is already loaded
if (document.readyState === 'loading') {
  // DOM is still loading, wait for DOMContentLoaded
} else {
  // DOM is already loaded
  initializeFsmMessages();
}