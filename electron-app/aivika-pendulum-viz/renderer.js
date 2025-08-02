// Global variables
let simulationData = [];
let animationFrameId = null;
let isPlaying = false;
let currentTimeIndex = 0;
let charts = {};

// UI Elements
const runBtn = document.getElementById("run-btn");
const resetBtn = document.getElementById("reset-btn");
const playPauseBtn = document.getElementById("play-pause-btn");
const pendulumLengthInput = document.getElementById("pendulum-length");
const dampingCoeffInput = document.getElementById("damping-coeff");
const initialAngleInput = document.getElementById("initial-angle");
const initialVelocityInput = document.getElementById("initial-velocity");
const timeEndInput = document.getElementById("time-end");
const statusDisplay = document.getElementById("status-display");
const progressFill = document.getElementById("progress-fill");

// Connection status elements
const connectionStatus = document.getElementById("connection-status");
const connectionText = document.getElementById("connection-text");
const testConnectionBtn = document.getElementById("test-connection-btn");

// Animation elements
const pendulumRod = document.getElementById("pendulum-rod");
const pendulumBob = document.getElementById("pendulum-bob");
const pendulumTrail = document.getElementById("pendulum-trail");
const currentTimeDisplay = document.getElementById("current-time");
const timeSlider = document.getElementById("time-slider");

// Tab elements
const tabBtns = document.querySelectorAll(".tab-btn");
const tabPanels = document.querySelectorAll(".tab-panel");

// Data table elements
const dataTableBody = document.getElementById("data-table-body");
const exportCsvBtn = document.getElementById("export-csv-btn");
const exportJsonBtn = document.getElementById("export-json-btn");

// Chart canvas elements
const angleChart = document.getElementById("angle-chart");
const velocityChart = document.getElementById("velocity-chart");
const phaseChart = document.getElementById("phase-chart");
const energyChart = document.getElementById("energy-chart");

// Connection status management
function updateConnectionStatus(status, message) {
  connectionStatus.className = `status-indicator ${status}`;
  connectionText.textContent = message;

  if (status === "connected") {
    runBtn.disabled = false;
    validateInputs();
  } else if (status === "disconnected") {
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
    const testResult = await window.electronAPI.runSimulation({
      pendulumLength: 1.0,
      dampingCoeff: 0.1,
      initialAngle: 45,
      initialVelocity: 0.0,
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

  setupEventListeners();
  resetToDefaults();

  // Initialize charts after a short delay
  setTimeout(() => {
    initializeCharts();
  }, 100);

  // Test server connection
  setTimeout(() => {
    testServerConnection();
  }, 1000);
});

// Setup event listeners
function setupEventListeners() {
  runBtn.addEventListener("click", runSimulation);
  resetBtn.addEventListener("click", resetToDefaults);
  playPauseBtn.addEventListener("click", togglePlayPause);
  testConnectionBtn.addEventListener("click", testServerConnection);

  // Tab switching
  tabBtns.forEach((btn) => {
    btn.addEventListener("click", () => {
      const tabName = btn.id.replace("-tab", "");
      switchTab(tabName);
    });
  });

  // Export buttons
  exportCsvBtn.addEventListener("click", exportToCsv);
  exportJsonBtn.addEventListener("click", exportToJson);

  // Input validation
  [
    pendulumLengthInput,
    dampingCoeffInput,
    initialAngleInput,
    initialVelocityInput,
    timeEndInput,
  ].forEach((input) => {
    input.addEventListener("input", validateInputs);
  });

  // Time slider
  timeSlider.addEventListener("input", (e) => {
    if (simulationData.length > 0) {
      currentTimeIndex = Math.floor(
        (e.target.value / 100) * (simulationData.length - 1),
      );
      updatePendulumPosition(currentTimeIndex);
      updateCurrentTimeDisplay();
    }
  });
}

// Initialize Chart.js charts
function initializeCharts() {
  console.log("Initializing charts...");

  if (typeof Chart === "undefined") {
    console.error("Chart.js is not loaded!");
    statusDisplay.textContent = "Error: Chart.js library not loaded";
    statusDisplay.style.color = "#e53e3e";
    return;
  }

  // Angle vs Time chart
  charts.angle = new Chart(angleChart.getContext("2d"), {
    type: "line",
    data: {
      labels: [],
      datasets: [
        {
          label: "Angle (degrees)",
          data: [],
          borderColor: "#667eea",
          backgroundColor: "rgba(102, 126, 234, 0.1)",
          borderWidth: 2,
          fill: true,
          tension: 0.4,
        },
      ],
    },
    options: createChartOptions("Angle vs Time", "Time (s)", "Angle (degrees)"),
  });

  // Angular Velocity chart
  charts.velocity = new Chart(velocityChart.getContext("2d"), {
    type: "line",
    data: {
      labels: [],
      datasets: [
        {
          label: "Angular Velocity (rad/s)",
          data: [],
          borderColor: "#48bb78",
          backgroundColor: "rgba(72, 187, 120, 0.1)",
          borderWidth: 2,
          fill: true,
          tension: 0.4,
        },
      ],
    },
    options: createChartOptions(
      "Angular Velocity vs Time",
      "Time (s)",
      "Angular Velocity (rad/s)",
    ),
  });

  // Phase Portrait chart
  charts.phase = new Chart(phaseChart.getContext("2d"), {
    type: "scatter",
    data: {
      datasets: [
        {
          label: "Phase Portrait",
          data: [],
          borderColor: "#ed64a6",
          backgroundColor: "#ed64a6",
          pointRadius: 2,
          showLine: true,
          fill: false,
          tension: 0,
        },
      ],
    },
    options: createChartOptions(
      "Phase Portrait",
      "Angle (degrees)",
      "Angular Velocity (rad/s)",
    ),
  });

  // Energy chart
  charts.energy = new Chart(energyChart.getContext("2d"), {
    type: "line",
    data: {
      labels: [],
      datasets: [
        {
          label: "Kinetic Energy",
          data: [],
          borderColor: "#f56565",
          backgroundColor: "rgba(245, 101, 101, 0.1)",
          borderWidth: 2,
          fill: false,
          tension: 0.4,
        },
        {
          label: "Potential Energy",
          data: [],
          borderColor: "#4299e1",
          backgroundColor: "rgba(66, 153, 225, 0.1)",
          borderWidth: 2,
          fill: false,
          tension: 0.4,
        },
        {
          label: "Total Energy",
          data: [],
          borderColor: "#38b2ac",
          backgroundColor: "rgba(56, 178, 172, 0.1)",
          borderWidth: 2,
          fill: false,
          tension: 0.4,
        },
      ],
    },
    options: createChartOptions("Energy vs Time", "Time (s)", "Energy (J)"),
  });
}

function createChartOptions(title, xLabel, yLabel) {
  return {
    responsive: true,
    maintainAspectRatio: false,
    plugins: {
      title: {
        display: true,
        text: title,
        font: { size: 16, weight: "bold" },
        color: "#4a5568",
      },
      legend: {
        display: true,
        position: "top",
      },
    },
    scales: {
      x: {
        title: {
          display: true,
          text: xLabel,
          font: { size: 12, weight: "bold" },
          color: "#4a5568",
        },
        grid: { color: "rgba(0,0,0,0.1)" },
      },
      y: {
        title: {
          display: true,
          text: yLabel,
          font: { size: 12, weight: "bold" },
          color: "#4a5568",
        },
        grid: { color: "rgba(0,0,0,0.1)" },
      },
    },
    interaction: {
      intersect: false,
      mode: "index",
    },
    animation: {
      duration: 500,
      easing: "easeInOutQuart",
    },
  };
}

// Switch between tabs
function switchTab(tabName) {
  tabBtns.forEach((btn) => btn.classList.remove("active"));
  document.getElementById(`${tabName}-tab`).classList.add("active");

  tabPanels.forEach((panel) => panel.classList.remove("active"));
  document.getElementById(`${tabName}-panel`).classList.add("active");

  // Resize chart if switching to chart tab
  if (charts[tabName]) {
    setTimeout(() => {
      try {
        charts[tabName].resize();
      } catch (e) {
        console.warn("Chart resize failed:", e);
      }
    }, 100);
  }
}

// Validate input values
function validateInputs() {
  const length = parseFloat(pendulumLengthInput.value);
  const damping = parseFloat(dampingCoeffInput.value);
  const angle = parseFloat(initialAngleInput.value);
  const velocity = parseFloat(initialVelocityInput.value);
  const timeEnd = parseFloat(timeEndInput.value);

  const isValid =
    length > 0 &&
    damping >= 0 &&
    Math.abs(angle) <= 90 &&
    Math.abs(velocity) <= 10 &&
    timeEnd > 0;

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
    pendulumLength: parseFloat(pendulumLengthInput.value),
    dampingCoeff: parseFloat(dampingCoeffInput.value),
    initialAngle: parseFloat(initialAngleInput.value),
    initialVelocity: parseFloat(initialVelocityInput.value),
    timeEnd: parseFloat(timeEndInput.value),
  };

  // Update UI for running state
  runBtn.disabled = true;
  playPauseBtn.disabled = true;
  statusDisplay.textContent = "Running simulation...";
  statusDisplay.style.color = "#667eea";
  progressFill.style.width = "50%";

  console.log("Starting simulation with params:", params);

  try {
    // Generate simulation data using Haskell backend or fallback
    const results = await generateSimulationData(params);
    console.log("Simulation results:", results);

    simulationData = results;
    currentTimeIndex = 0;

    // Update UI with results
    updateCharts(results, params);
    updateDataTable(results);
    setupPendulumAnimation(results, params);

    // Enable controls
    playPauseBtn.disabled = false;
    exportCsvBtn.disabled = false;
    exportJsonBtn.disabled = false;
    timeSlider.disabled = false;
    timeSlider.max = results.length - 1;

    statusDisplay.textContent = `Simulation complete! Generated ${results.length} data points.`;
    statusDisplay.style.color = "#48bb78";
    progressFill.style.width = "100%";

    // Switch to angle chart to show results
    switchTab("angle");
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

// Generate simulation data
async function generateSimulationData(params) {
  console.log("generateSimulationData called with:", params);

  if (!window.electronAPI) {
    console.error("electronAPI not available!");
    throw new Error("electronAPI not available");
  }

  try {
    console.log("Calling window.electronAPI.runSimulation...");
    const results = await window.electronAPI.runSimulation(params);
    console.log("Haskell simulation succeeded:", results);

    updateConnectionStatus("connected", "Connected to Haskell server");
    return results;
  } catch (error) {
    console.error("Failed to run Haskell simulation:", error);
    console.log("Falling back to JavaScript simulation");

    updateConnectionStatus(
      "disconnected",
      "Server offline (using fallback simulation)",
    );

    // Fallback to JavaScript simulation
    return generateFallbackSimulation(params);
  }
}

// JavaScript fallback simulation
function generateFallbackSimulation(params) {
  const {
    pendulumLength,
    dampingCoeff,
    initialAngle,
    initialVelocity,
    timeEnd,
  } = params;
  const dataPoints = [];
  const dt = 0.05; // Time step
  const g = 9.81; // Gravity

  let theta = (initialAngle * Math.PI) / 180; // Convert to radians
  let omega = initialVelocity;

  for (let t = 0; t <= timeEnd; t += dt) {
    // Simple Euler integration for pendulum equation
    const alpha =
      -(g / pendulumLength) * Math.sin(theta) - dampingCoeff * omega;

    // Calculate positions
    const x = pendulumLength * Math.sin(theta);
    const y = -pendulumLength * Math.cos(theta);

    dataPoints.push({
      time: Math.round(t * 100) / 100,
      angle: theta,
      angularVelocity: omega,
      xPosition: x,
      yPosition: y,
    });

    // Update for next iteration
    omega += alpha * dt;
    theta += omega * dt;
  }

  console.log(
    "JavaScript fallback generated:",
    dataPoints.length,
    "data points",
  );
  return dataPoints;
}

// Setup pendulum animation
function setupPendulumAnimation(data, params) {
  const svgRect = document
    .getElementById("pendulum-svg")
    .getBoundingClientRect();
  const centerX = 200; // SVG center X
  const centerY = 50; // SVG pivot Y
  const scale = 100 / params.pendulumLength; // Scale factor for visualization

  // Update pendulum position
  updatePendulumPosition(0);
  updateCurrentTimeDisplay();

  // Clear any existing trail
  pendulumTrail.setAttribute("d", "");
}

// Update pendulum position for given time index
function updatePendulumPosition(timeIndex) {
  if (!simulationData || simulationData.length === 0) return;

  const data = simulationData[timeIndex];
  const centerX = 200;
  const centerY = 50;
  const scale = 100; // Fixed scale for visualization

  // Calculate pendulum position
  const x = centerX + data.xPosition * scale;
  const y = centerY - data.yPosition * scale; // Negative because SVG Y is inverted

  // Update rod
  pendulumRod.setAttribute("x2", x);
  pendulumRod.setAttribute("y2", y);

  // Update bob
  pendulumBob.setAttribute("cx", x);
  pendulumBob.setAttribute("cy", y);

  // Update trail
  updateTrail(timeIndex);

  // Update time slider
  timeSlider.value = (timeIndex / (simulationData.length - 1)) * 100;
}

// Update pendulum trail
function updateTrail(currentIndex) {
  if (currentIndex < 5) return; // Don't show trail for first few points

  const centerX = 200;
  const centerY = 50;
  const scale = 100;

  let pathData = "";
  const trailLength = Math.min(50, currentIndex); // Show last 50 points

  for (let i = currentIndex - trailLength; i <= currentIndex; i++) {
    const data = simulationData[i];
    const x = centerX + data.xPosition * scale;
    const y = centerY - data.yPosition * scale;

    if (i === currentIndex - trailLength) {
      pathData += `M ${x} ${y}`;
    } else {
      pathData += ` L ${x} ${y}`;
    }
  }

  pendulumTrail.setAttribute("d", pathData);
}

// Update current time display
function updateCurrentTimeDisplay() {
  if (simulationData.length > 0) {
    const currentTime = simulationData[currentTimeIndex].time;
    currentTimeDisplay.textContent = currentTime.toFixed(2);
  }
}

// Toggle play/pause animation
function togglePlayPause() {
  if (isPlaying) {
    stopAnimation();
  } else {
    startAnimation();
  }
}

// Start animation
function startAnimation() {
  if (simulationData.length === 0) return;

  isPlaying = true;
  playPauseBtn.textContent = "⏸ Pause";

  function animate() {
    if (!isPlaying) return;

    currentTimeIndex = (currentTimeIndex + 1) % simulationData.length;
    updatePendulumPosition(currentTimeIndex);
    updateCurrentTimeDisplay();

    // Continue animation
    animationFrameId = requestAnimationFrame(() => {
      setTimeout(animate, 50); // 20 FPS
    });
  }

  animate();
}

// Stop animation
function stopAnimation() {
  isPlaying = false;
  playPauseBtn.textContent = "▶ Play";

  if (animationFrameId) {
    cancelAnimationFrame(animationFrameId);
    animationFrameId = null;
  }
}

// Update charts with simulation data
function updateCharts(data, params) {
  const times = data.map((d) => d.time);
  const angles = data.map((d) => (d.angle * 180) / Math.PI); // Convert to degrees
  const velocities = data.map((d) => d.angularVelocity);

  // Update angle chart
  charts.angle.data.labels = times;
  charts.angle.data.datasets[0].data = angles;
  charts.angle.update();

  // Update velocity chart
  charts.velocity.data.labels = times;
  charts.velocity.data.datasets[0].data = velocities;
  charts.velocity.update();

  // Update phase portrait
  const phaseData = data.map((d) => ({
    x: (d.angle * 180) / Math.PI,
    y: d.angularVelocity,
  }));
  charts.phase.data.datasets[0].data = phaseData;
  charts.phase.update();

  // Calculate and update energy chart
  const energyData = calculateEnergy(data, params);
  charts.energy.data.labels = times;
  charts.energy.data.datasets[0].data = energyData.kinetic;
  charts.energy.data.datasets[1].data = energyData.potential;
  charts.energy.data.datasets[2].data = energyData.total;
  charts.energy.update();
}

// Calculate energy values
function calculateEnergy(data, params) {
  const g = 9.81;
  const m = 1; // Assume unit mass
  const L = params.pendulumLength;

  const kinetic = data.map(
    (d) => 0.5 * m * L * L * d.angularVelocity * d.angularVelocity,
  );
  const potential = data.map((d) => m * g * L * (1 - Math.cos(d.angle)));
  const total = kinetic.map((k, i) => k + potential[i]);

  return { kinetic, potential, total };
}

// Update data table
function updateDataTable(data) {
  dataTableBody.innerHTML = "";

  // Sample every 10th point to avoid too many rows
  const sampledData = data.filter((_, index) => index % 10 === 0);

  sampledData.forEach((point) => {
    const row = document.createElement("tr");
    row.innerHTML = `
      <td>${point.time.toFixed(2)}</td>
      <td>${((point.angle * 180) / Math.PI).toFixed(2)}</td>
      <td>${point.angularVelocity.toFixed(3)}</td>
      <td>${point.xPosition.toFixed(3)}</td>
      <td>${point.yPosition.toFixed(3)}</td>
    `;
    dataTableBody.appendChild(row);
  });
}

// Export data to CSV
function exportToCsv() {
  if (simulationData.length === 0) return;

  let csv =
    "Time (s),Angle (degrees),Angular Velocity (rad/s),X Position (m),Y Position (m)\n";
  simulationData.forEach((point) => {
    csv += `${point.time},${(point.angle * 180) / Math.PI},${point.angularVelocity},${point.xPosition},${point.yPosition}\n`;
  });

  downloadFile(csv, "pendulum_simulation.csv", "text/csv");
}

// Export data to JSON
function exportToJson() {
  if (simulationData.length === 0) return;

  const json = JSON.stringify(simulationData, null, 2);
  downloadFile(json, "pendulum_simulation.json", "application/json");
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
  pendulumLengthInput.value = "1.0";
  dampingCoeffInput.value = "0.1";
  initialAngleInput.value = "45";
  initialVelocityInput.value = "0.0";
  timeEndInput.value = "10";

  statusDisplay.textContent = "Ready to run simulation";
  statusDisplay.style.color = "#4a5568";
  progressFill.style.width = "0%";

  // Clear animation
  stopAnimation();
  currentTimeIndex = 0;

  // Clear data
  simulationData = [];

  // Reset pendulum to initial position
  pendulumRod.setAttribute("x2", "200");
  pendulumRod.setAttribute("y2", "150");
  pendulumBob.setAttribute("cx", "200");
  pendulumBob.setAttribute("cy", "150");
  pendulumTrail.setAttribute("d", "");

  currentTimeDisplay.textContent = "0.00";
  timeSlider.value = "0";
  timeSlider.disabled = true;
  playPauseBtn.disabled = true;
  playPauseBtn.textContent = "▶ Play";

  // Clear charts
  Object.values(charts).forEach((chart) => {
    if (chart) {
      chart.data.labels = [];
      chart.data.datasets.forEach((dataset) => {
        dataset.data = [];
      });
      chart.update();
    }
  });

  dataTableBody.innerHTML = "";
  exportCsvBtn.disabled = true;
  exportJsonBtn.disabled = true;

  validateInputs();
}
