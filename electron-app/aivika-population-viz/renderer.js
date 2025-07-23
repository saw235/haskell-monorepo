// Global variables
let populationChart = null;
let simulationData = [];

// UI Elements
const runBtn = document.getElementById("run-btn");
const resetBtn = document.getElementById("reset-btn");
const initialPopInput = document.getElementById("initial-pop");
const growthRateInput = document.getElementById("growth-rate");
const timeEndInput = document.getElementById("time-end");
const statusDisplay = document.getElementById("status-display");
const progressFill = document.getElementById("progress-fill");

// Connection status elements
const connectionStatus = document.getElementById("connection-status");
const connectionText = document.getElementById("connection-text");
const testConnectionBtn = document.getElementById("test-connection-btn");

// Tab elements
const tabBtns = document.querySelectorAll(".tab-btn");
const tabPanels = document.querySelectorAll(".tab-panel");

// Chart canvas
const chartCanvas = document.getElementById("population-chart");

// Data table elements
const dataTableBody = document.getElementById("data-table-body");
const exportCsvBtn = document.getElementById("export-csv-btn");
const exportJsonBtn = document.getElementById("export-json-btn");

// Analysis elements
const statInitial = document.getElementById("stat-initial");
const statFinal = document.getElementById("stat-final");
const statGrowth = document.getElementById("stat-growth");
const statDoubling = document.getElementById("stat-doubling");

// Connection status management
function updateConnectionStatus(status, message) {
  connectionStatus.className = `status-indicator ${status}`;
  connectionText.textContent = message;

  // Update run button availability based on connection
  if (status === "connected") {
    runBtn.disabled = false;
    validateInputs(); // Re-validate in case inputs are valid
  } else if (status === "disconnected") {
    // Keep button enabled for fallback simulation
    console.log(
      "Server disconnected, simulations will use JavaScript fallback",
    );
  }
}

// Test server connection
async function testServerConnection() {
  updateConnectionStatus("connecting", "Testing server connection...");
  testConnectionBtn.disabled = true;

  try {
    // Try a quick ping to the server
    const testResult = await window.electronAPI.runSimulation({
      initialPop: 1000,
      growthRate: 5,
      timeEnd: 1,
    });

    if (testResult && testResult.length > 0) {
      updateConnectionStatus("connected", "Connected to Haskell server");
      return true;
    } else {
      throw new Error("Invalid server response");
    }
  } catch (error) {
    console.warn("Server connection test failed:", error.message);
    updateConnectionStatus(
      "disconnected",
      "Server offline (using fallback simulation)",
    );
    return false;
  } finally {
    testConnectionBtn.disabled = false;
  }
}

// Initialize the app
document.addEventListener("DOMContentLoaded", () => {
  console.log("DOM loaded, initializing app...");

  // Check if all required elements exist
  if (!chartCanvas) {
    console.error("Chart canvas not found!");
    return;
  }

  setupEventListeners();
  resetToDefaults();

  // Initialize chart after a short delay to ensure Chart.js is loaded
  setTimeout(() => {
    initializeChart();
  }, 100);

  // Test server connection after a brief delay
  setTimeout(() => {
    testServerConnection();
  }, 1000);
});

// Setup event listeners
function setupEventListeners() {
  runBtn.addEventListener("click", runSimulation);
  resetBtn.addEventListener("click", resetToDefaults);
  testConnectionBtn.addEventListener("click", testServerConnection);

  // Tab switching
  tabBtns.forEach((btn) => {
    btn.addEventListener("click", () => switchTab(btn.id.replace("-tab", "")));
  });

  // Export buttons
  exportCsvBtn.addEventListener("click", exportToCsv);
  exportJsonBtn.addEventListener("click", exportToJson);

  // Input validation
  [initialPopInput, growthRateInput, timeEndInput].forEach((input) => {
    input.addEventListener("input", validateInputs);
  });
}

// Initialize Chart.js
function initializeChart() {
  console.log("Initializing chart...");

  if (typeof Chart === "undefined") {
    console.error("Chart.js is not loaded!");
    statusDisplay.textContent = "Error: Chart.js library not loaded";
    statusDisplay.style.color = "#e53e3e";
    return;
  }

  const ctx = chartCanvas.getContext("2d");

  populationChart = new Chart(ctx, {
    type: "line",
    data: {
      labels: [],
      datasets: [
        {
          label: "Population",
          data: [],
          borderColor: "#667eea",
          backgroundColor: "rgba(102, 126, 234, 0.1)",
          borderWidth: 3,
          fill: true,
          tension: 0.4,
          pointBackgroundColor: "#667eea",
          pointBorderColor: "#ffffff",
          pointBorderWidth: 2,
          pointRadius: 6,
          pointHoverRadius: 8,
        },
      ],
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        title: {
          display: true,
          text: "Population Growth Over Time",
          font: {
            size: 18,
            weight: "bold",
          },
          color: "#4a5568",
        },
        legend: {
          display: false,
        },
      },
      scales: {
        x: {
          title: {
            display: true,
            text: "Time (years)",
            font: {
              size: 14,
              weight: "bold",
            },
            color: "#4a5568",
          },
          grid: {
            color: "rgba(0,0,0,0.1)",
          },
        },
        y: {
          title: {
            display: true,
            text: "Population",
            font: {
              size: 14,
              weight: "bold",
            },
            color: "#4a5568",
          },
          grid: {
            color: "rgba(0,0,0,0.1)",
          },
          beginAtZero: true,
        },
      },
      interaction: {
        intersect: false,
        mode: "index",
      },
      animation: {
        duration: 1000,
        easing: "easeInOutQuart",
      },
    },
  });
}

// Switch between tabs
function switchTab(tabName) {
  // Update tab buttons
  tabBtns.forEach((btn) => btn.classList.remove("active"));
  document.getElementById(`${tabName}-tab`).classList.add("active");

  // Update tab panels
  tabPanels.forEach((panel) => panel.classList.remove("active"));
  document.getElementById(`${tabName}-panel`).classList.add("active");

  // Resize chart if switching to chart tab
  if (tabName === "chart" && populationChart) {
    setTimeout(() => {
      try {
        populationChart.resize();
      } catch (e) {
        console.warn("Chart resize failed:", e);
      }
    }, 100);
  }
}

// Validate input values
function validateInputs() {
  const initialPop = parseFloat(initialPopInput.value);
  const growthRate = parseFloat(growthRateInput.value);
  const timeEnd = parseFloat(timeEndInput.value);

  const isValid = initialPop > 0 && growthRate >= 0 && timeEnd > 0;
  runBtn.disabled = !isValid;

  if (!isValid) {
    statusDisplay.textContent = "Please check input values";
    statusDisplay.style.color = "#e53e3e";
  } else {
    statusDisplay.textContent = "Ready to run simulation";
    statusDisplay.style.color = "#4a5568";
  }
}

// Run the simulation
async function runSimulation() {
  const params = {
    initialPop: parseFloat(initialPopInput.value),
    growthRate: parseFloat(growthRateInput.value), // Keep as percentage for now
    timeEnd: parseFloat(timeEndInput.value),
  };

  // Update UI for running state
  runBtn.disabled = true;
  statusDisplay.textContent = "Running simulation...";
  statusDisplay.style.color = "#667eea";
  progressFill.style.width = "50%";

  console.log("Starting simulation with params:", params);

  try {
    // Generate simulation data using Haskell backend
    const results = await generateSimulationData(params);
    console.log("Simulation results:", results);

    simulationData = results;

    // Update UI with results
    updateChart(results);
    updateDataTable(results);
    updateAnalysis(results, params);

    // Enable export buttons
    exportCsvBtn.disabled = false;
    exportJsonBtn.disabled = false;

    statusDisplay.textContent = `Simulation complete! Generated ${results.length} data points.`;
    statusDisplay.style.color = "#48bb78";
    progressFill.style.width = "100%";

    // Switch to chart tab to show results
    switchTab("chart");
  } catch (error) {
    console.error("Simulation error:", error);
    statusDisplay.textContent = `Simulation failed: ${error.message}`;
    statusDisplay.style.color = "#e53e3e";
    progressFill.style.width = "0%";
  } finally {
    runBtn.disabled = false;
    setTimeout(() => {
      progressFill.style.width = "0%";
    }, 2000);
  }
}

// Generate simulation data using Haskell backend
async function generateSimulationData(params) {
  console.log("generateSimulationData called with:", params);

  if (!window.electronAPI) {
    console.error("electronAPI not available!");
    throw new Error("electronAPI not available");
  }

  try {
    console.log("Calling window.electronAPI.runSimulation...");
    // Call the Haskell simulation through Electron's main process
    const results = await window.electronAPI.runSimulation(params);
    console.log("Haskell simulation succeeded:", results);

    // Update connection status on successful simulation
    updateConnectionStatus("connected", "Connected to Haskell server");

    return results;
  } catch (error) {
    console.error("Failed to run Haskell simulation:", error);
    console.log("Falling back to JavaScript simulation");

    // Update connection status to show we're using fallback
    updateConnectionStatus(
      "disconnected",
      "Server offline (using fallback simulation)",
    );

    // Fallback to JavaScript simulation if Haskell fails
    const { initialPop, growthRate, timeEnd } = params;
    const dataPoints = [];
    const dt = 0.5; // Time step (matches Haskell output)

    for (let t = 0; t <= timeEnd; t += dt) {
      const population = initialPop * Math.exp((growthRate / 100) * t);
      dataPoints.push({
        time: Math.round(t * 10) / 10, // Round to 1 decimal place
        population: Math.round(population),
      });
    }

    console.log(
      "JavaScript fallback generated:",
      dataPoints.length,
      "data points",
    );
    return dataPoints;
  }
}

// Update the chart with new data
function updateChart(data) {
  if (!populationChart) {
    console.warn("Chart not initialized, skipping chart update");
    return;
  }

  const labels = data.map((point) => point.time);
  const populations = data.map((point) => point.population);

  populationChart.data.labels = labels;
  populationChart.data.datasets[0].data = populations;
  populationChart.update();
}

// Update the data table
function updateDataTable(data) {
  dataTableBody.innerHTML = "";

  // Sample every 10th point to avoid too many rows
  const sampledData = data.filter((_, index) => index % 10 === 0);

  sampledData.forEach((point, index) => {
    const row = document.createElement("tr");
    const growth =
      index > 0
        ? (
            ((point.population - sampledData[index - 1].population) /
              sampledData[index - 1].population) *
            100
          ).toFixed(2) + "%"
        : "-";

    row.innerHTML = `
            <td>${point.time.toFixed(1)}</td>
            <td>${point.population.toLocaleString()}</td>
            <td>${growth}</td>
        `;
    dataTableBody.appendChild(row);
  });
}

// Update analysis statistics
function updateAnalysis(data, params) {
  const initialPop = data[0].population;
  const finalPop = data[data.length - 1].population;
  const totalGrowth = ((finalPop - initialPop) / initialPop) * 100;
  const doublingTime = Math.log(2) / params.growthRate;

  statInitial.textContent = initialPop.toLocaleString();
  statFinal.textContent = finalPop.toLocaleString();
  statGrowth.textContent = totalGrowth.toFixed(1) + "%";
  statDoubling.textContent = doublingTime.toFixed(1) + " years";
}

// Export data to CSV
function exportToCsv() {
  if (simulationData.length === 0) return;

  let csv = "Time (years),Population\n";
  simulationData.forEach((point) => {
    csv += `${point.time},${point.population}\n`;
  });

  downloadFile(csv, "population_growth.csv", "text/csv");
}

// Export data to JSON
function exportToJson() {
  if (simulationData.length === 0) return;

  const json = JSON.stringify(simulationData, null, 2);
  downloadFile(json, "population_growth.json", "application/json");
}

// Download file helper
function downloadFile(content, filename, contentType) {
  const blob = new Blob([content], { type: contentType });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
}

// Reset all inputs to defaults
function resetToDefaults() {
  initialPopInput.value = "1000";
  growthRateInput.value = "5";
  timeEndInput.value = "20";

  statusDisplay.textContent = "Ready to run simulation";
  statusDisplay.style.color = "#4a5568";
  progressFill.style.width = "0%";

  // Clear previous results
  simulationData = [];
  if (populationChart) {
    try {
      populationChart.data.labels = [];
      populationChart.data.datasets[0].data = [];
      populationChart.update();
    } catch (e) {
      console.warn("Chart reset failed:", e);
    }
  }

  dataTableBody.innerHTML = "";
  exportCsvBtn.disabled = true;
  exportJsonBtn.disabled = true;

  // Reset statistics
  statInitial.textContent = "-";
  statFinal.textContent = "-";
  statGrowth.textContent = "-";
  statDoubling.textContent = "-";

  validateInputs();
}
