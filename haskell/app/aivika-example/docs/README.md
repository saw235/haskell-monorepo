# Aivika Library Documentation

This directory contains comprehensive documentation for the [Aivika](https://hackage.haskell.org/package/aivika) Haskell simulation library, organized by module and functionality.

## 📚 Documentation Structure

### **Core Documentation**
- **[01-overview.md](01-overview.md)** - Library overview, key features, and use cases
- **[02-core-simulation.md](02-core-simulation.md)** - Core simulation framework and basic functions
- **[03-system-dynamics.md](03-system-dynamics.md)** - Differential equation solving and continuous systems
- **[04-results-and-output.md](04-results-and-output.md)** - Results collection, analysis, and output formatting
- **[05-queue-and-resource-management.md](05-queue-and-resource-management.md)** - Queue and resource management for discrete event simulation
- **[06-process-and-statistics.md](06-process-and-statistics.md)** - Process modeling and statistical analysis
- **[07-practical-examples-and-index.md](07-practical-examples-and-index.md)** - Real-world examples and quick reference

## 🎯 What is Aivika?

Aivika is a powerful Haskell library for **discrete event simulation** and **system dynamics modeling**. It provides:

- **Discrete Event Simulation**: Model systems with discrete state changes
- **System Dynamics**: Solve differential equations for continuous processes
- **Hybrid Simulation**: Combine discrete and continuous modeling
- **Type-Safe Programming**: Strong typing prevents simulation errors
- **Extensible Architecture**: Modular design for custom extensions

## 🚀 Getting Started

### **Quick Start Example**
```haskell
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Results

-- Define simulation parameters
specs = Specs 
  { spcStartTime = 0.0
  , spcStopTime = 10.0
  , spcDT = 0.1
  , spcMethod = RungeKutta4
  , spcGeneratorType = SimpleGenerator
  }

-- Simple simulation model
model :: Simulation Results
model = do
  x <- integ (return 1.0) 0.0  -- dx/dt = 1, x(0) = 0
  return $ results [resultSource "x" "position" x]

-- Run simulation
main = runSimulation specs model
```

### **Building and Running**
```bash
# Build the example
bazel build //haskell/app/aivika-example:aivika-example

# Run the example
bazel run //haskell/app/aivika-example:aivika-example
```

## 📖 Learning Path

### **For Beginners**
1. Start with **[01-overview.md](01-overview.md)** to understand the library's capabilities
2. Read **[02-core-simulation.md](02-core-simulation.md)** for basic simulation concepts
3. Try the bouncing ball example in the main directory
4. Explore **[07-practical-examples-and-index.md](07-practical-examples-and-index.md)** for real-world examples

### **For Intermediate Users**
1. Study **[03-system-dynamics.md](03-system-dynamics.md)** for continuous systems
2. Learn **[05-queue-and-resource-management.md](05-queue-and-resource-management.md)** for discrete event simulation
3. Master **[04-results-and-output.md](04-results-and-output.md)** for data analysis
4. Practice with the comprehensive examples in **[07-practical-examples-and-index.md](07-practical-examples-and-index.md)**

### **For Advanced Users**
1. Explore **[06-process-and-statistics.md](06-process-and-statistics.md)** for complex workflows
2. Study the advanced examples in the practical examples document
3. Customize and extend the library for your specific needs

## 🔧 Key Modules

### **Core Framework**
- `Simulation.Aivika` - Main simulation framework
- `Simulation.Aivika.SystemDynamics` - Differential equation solvers
- `Simulation.Aivika.Results` - Result collection and analysis

### **Discrete Event Simulation**
- `Simulation.Aivika.Queue` - Queue management (FCFS, LCFS, Priority)
- `Simulation.Aivika.Resource` - Resource allocation and management
- `Simulation.Aivika.Process` - Process modeling and workflow management

### **Analysis and Output**
- `Simulation.Aivika.Statistics` - Statistical analysis and data collection
- `Simulation.Aivika.Results.IO` - Input/output operations and formatting
- `Simulation.Aivika.Results.Charting` - Chart generation and visualization

## 💡 Common Use Cases

### **Business Process Simulation**
- Queue management systems
- Resource allocation
- Process optimization
- Performance analysis

### **Physical System Modeling**
- Mechanical systems
- Electrical circuits
- Chemical processes
- Biological systems

### **Economic Modeling**
- Market simulations
- Supply chain analysis
- Risk assessment
- Policy evaluation

### **Scientific Research**
- Population dynamics
- Climate modeling
- Particle physics
- Epidemiology

## 🛠️ Examples Included

### **Basic Examples**
- Bouncing ball simulation (in main directory)
- Simple harmonic oscillator
- Exponential decay
- First-order chemical reaction

### **Advanced Examples**
- Manufacturing system simulation
- Hospital emergency department
- Supply chain management
- Financial market simulation

## 📊 Output Formats

Aivika supports multiple output formats:

- **Text Output**: Human-readable console output
- **CSV Files**: For data analysis in spreadsheets
- **JSON Files**: For web applications and APIs
- **Custom Formats**: Extensible output system

## 🔍 Statistical Analysis

The library provides comprehensive statistical capabilities:

- **Basic Statistics**: Mean, variance, standard deviation, median
- **Advanced Statistics**: Percentiles, skewness, kurtosis
- **Confidence Intervals**: Statistical significance testing
- **Real-time Monitoring**: Live statistical analysis during simulation

## ⚡ Performance Considerations

### **Optimization Tips**
- Use appropriate integration methods (RK4 recommended)
- Choose suitable time steps for your system
- Minimize discrete events in continuous simulations
- Use lazy evaluation for large result sets
- Profile performance for critical applications

### **Memory Management**
- Use bounded queues for large simulations
- Stream results to files for very large datasets
- Consider using external databases for massive datasets
- Monitor memory usage during long simulations

## 🤝 Contributing

This documentation is part of the Aivika example project. To contribute:

1. Study the existing examples and documentation
2. Create new examples for your use cases
3. Improve documentation clarity and completeness
4. Add new modules or functionality as needed

## 📚 Additional Resources

- **[Aivika Hackage Package](https://hackage.haskell.org/package/aivika)** - Official package documentation
- **[Aivika GitHub Repository](https://github.com/dsorokin/aivika)** - Source code and issues
- **[Haskell Documentation](https://www.haskell.org/documentation/)** - General Haskell resources

## 🎓 Learning Resources

### **Simulation Theory**
- Discrete Event Simulation principles
- System Dynamics methodology
- Queueing theory
- Statistical analysis for simulations

### **Haskell Programming**
- Monad transformers
- Type-safe programming
- Functional programming patterns
- Performance optimization

## 📝 Documentation Style

This documentation follows these principles:

- **Progressive Complexity**: Start simple, build to advanced
- **Practical Examples**: Real-world use cases with complete code
- **Type Safety**: Emphasize Haskell's type system benefits
- **Performance Awareness**: Include optimization guidance
- **Best Practices**: Industry-standard simulation practices

## 🔗 Related Projects

- **aivika-chart**: Chart generation for Aivika simulations
- **aivika-web**: Web-based visualization tools
- **aivika-statistics**: Extended statistical analysis
- **aivika-experiment**: Experimental design and analysis

---

*This documentation is maintained as part of the Aivika example project. For questions or contributions, please refer to the main project documentation.* 