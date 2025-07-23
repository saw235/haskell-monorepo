# Aivika Population Growth Simulation

A simple exponential population growth model implemented using Aivika's system dynamics capabilities.

## Overview

This example demonstrates how to model and simulate exponential population growth using differential equations. It serves as an introduction to:

- Basic Aivika simulation structure
- Differential equation integration with `integ`
- System dynamics modeling concepts
- Exponential growth patterns in nature

## Mathematical Model

The simulation models exponential population growth using the differential equation:

```
dP/dt = r × P
```

Where:
- **P** = population at time t
- **r** = growth rate (0.05 = 5% per year)
- **P(0)** = initial population (1000 individuals)

### Analytical Solution

The analytical solution to this differential equation is:
```
P(t) = P₀ × e^(r×t)
```

For our parameters:
- P(t) = 1000 × e^(0.05×t)
- At t=20 years: P(20) = 1000 × e^1.0 ≈ 2718 individuals

## Physical Interpretation

This model represents **exponential growth**, commonly observed in:

1. **Early bacterial growth** in unlimited media
2. **Population dynamics** with abundant resources
3. **Compound interest** in financial systems
4. **Nuclear chain reactions** in physics
5. **Viral spread** in initial phases of epidemics

### Key Characteristics

- **Growth rate is proportional to current population**
- **Doubling time**: t_double = ln(2)/r ≈ 13.86 years
- **No carrying capacity** (unrealistic for real populations)
- **Exponential curve**: slow start, rapid acceleration

## Simulation Parameters

| Parameter | Value | Unit |
|-----------|-------|------|
| Growth rate (r) | 0.05 | per year |
| Initial population | 1000 | individuals |
| Simulation time | 0-20 | years |
| Time step | 0.1 | years |
| Integration method | RungeKutta4 | - |

## Usage

### Build and Run

```bash
# Build the application
bazel build //haskell/app/aivika-population-growth:aivika-population-growth

# Run the simulation
bazel run //haskell/app/aivika-population-growth:aivika-population-growth
```

### Expected Output

```
Aivika Population Growth Simulation
==================================
Model: dP/dt = r * P
Growth rate (r): 5.0% per year
Initial population (P0): 1000
Simulation time: 0.0 to 20.0 years
Expected final population: ~2718

Results at key time points:
Time (years) | Population
-------------|------------
0.0          | 1000.0
5.0          | ~1284.0
10.0         | ~1649.0
15.0         | ~2117.0
20.0         | ~2718.0

Simulation complete!
```

## Learning Objectives

After running this example, you should understand:

1. **System dynamics modeling** with Aivika
2. **Differential equation integration** using `integ`
3. **Simulation specifications** setup
4. **Exponential growth behavior**
5. **Results output and visualization**

## Extension Ideas

Try modifying the model to explore:

1. **Logistic growth**: Add carrying capacity
2. **Different growth rates**: Compare r = 0.01, 0.03, 0.07
3. **Stochastic growth**: Add random fluctuations
4. **Predator-prey dynamics**: Two-species interaction
5. **Age-structured population**: Multiple age groups

## Code Structure

- `main`: Entry point and result display
- `model`: Core simulation logic with differential equation
- `specs`: Simulation configuration parameters
- Constants: `growthRate`, `initialPopulation`

## Mathematical Background

### Derivation

Starting from the assumption that growth rate is proportional to population:
```
Rate of change ∝ Current population
dP/dt = r × P
```

This is a **first-order linear ODE** with solution:
```
P(t) = P₀ × e^(rt)
```

### Numerical Integration

Aivika uses RungeKutta4 method to approximate:
```
P_{n+1} = P_n + h × f(t_n, P_n)
where f(t,P) = r × P
```

This provides accurate results for exponential growth patterns.

## References

- [Aivika Documentation](https://hackage.haskell.org/package/aivika)
- [System Dynamics Methodology](https://en.wikipedia.org/wiki/System_dynamics)
- [Exponential Growth in Biology](https://en.wikipedia.org/wiki/Exponential_growth)