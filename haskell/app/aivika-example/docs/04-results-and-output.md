# Results and Output Modules: Simulation.Aivika.Results

## Overview

The Results modules in Aivika provide comprehensive tools for collecting, analyzing, and outputting simulation results. These modules are essential for extracting meaningful information from simulations and presenting it in various formats.

## Key Modules

### **Core Results Module: `Simulation.Aivika.Results`**
- Result collection and management
- Statistical analysis
- Data aggregation

### **IO Module: `Simulation.Aivika.Results.IO`**
- Text-based output formatting
- File I/O operations
- Console output

### **Charting Module: `Simulation.Aivika.Results.Charting`**
- Chart generation
- Plotting capabilities
- Visualization tools

## Core Results Types

### **Results Data Structure**
```haskell
data Results = Results [ResultSource]

data ResultSource = ResultSource
  { sourceId :: String      -- Unique identifier
  , sourceName :: String    -- Human-readable name
  , sourceValue :: Simulation Double  -- The actual value
  }
```

### **Creating Results**
```haskell
results :: [ResultSource] -> Results

resultSource :: String -> String -> Simulation Double -> ResultSource
```

**Usage:**
```haskell
model = do
  x <- integ dx x0
  v <- integ dv v0
  
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "x" "position" x
    , resultSource "v" "velocity" v
    ]
```

## Results.IO Module Functions

### **Output Functions**

#### `printSimulationResultsInTimes`
```haskell
printSimulationResultsInTimes :: [Double] -> ResultPrinting -> Simulation Results -> Specs -> IO ()
```
Prints simulation results at specific time points.

**Parameters:**
- `[Double]`: List of time points to print results
- `ResultPrinting`: Output formatter function
- `Simulation Results`: The simulation model
- `Specs`: Simulation specifications

**Usage:**
```haskell
main = do
  let timePoints = [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]
  printSimulationResultsInTimes timePoints printResultSourceInEnglish model specs
```

#### `printSimulationResultsInStopTime`
```haskell
printSimulationResultsInStopTime :: ResultPrinting -> Simulation Results -> Specs -> IO ()
```
Prints simulation results at the final time.

**Usage:**
```haskell
main = do
  printSimulationResultsInStopTime printResultSourceInEnglish model specs
```

#### `printSimulationResultsInIntegTimes`
```haskell
printSimulationResultsInIntegTimes :: Double -> ResultPrinting -> Simulation Results -> Specs -> IO ()
```
Prints simulation results at regular intervals.

**Parameters:**
- `Double`: Time interval between outputs
- `ResultPrinting`: Output formatter function
- `Simulation Results`: The simulation model
- `Specs`: Simulation specifications

**Usage:**
```haskell
main = do
  printSimulationResultsInIntegTimes 0.5 printResultSourceInEnglish model specs
```

### **Output Formatters**

#### `printResultSourceInEnglish`
```haskell
printResultSourceInEnglish :: ResultSource -> IO ()
```
Formats and prints a single result source in English.

**Output Format:**
```
----------
-- simulation time
t = 0.0
-- time
t = 0.0
-- position
x = 10.0
-- velocity
v = 15.0
```

#### `printResultSourceInCSV`
```haskell
printResultSourceInCSV :: ResultSource -> IO ()
```
Formats and prints a single result source in CSV format.

**Output Format:**
```
t,x,v
0.0,10.0,15.0
0.5,16.519,10.095
1.0,20.637,5.19
```

### **File Output Functions**

#### `writeSimulationResultsInTimes`
```haskell
writeSimulationResultsInTimes :: FilePath -> [Double] -> ResultPrinting -> Simulation Results -> Specs -> IO ()
```
Writes simulation results to a file at specific time points.

**Usage:**
```haskell
main = do
  let timePoints = [0.0, 0.5, 1.0, 1.5, 2.0]
  writeSimulationResultsInTimes "results.txt" timePoints printResultSourceInEnglish model specs
```

#### `writeSimulationResultsInCSV`
```haskell
writeSimulationResultsInCSV :: FilePath -> [Double] -> Simulation Results -> Specs -> IO ()
```
Writes simulation results to a CSV file.

**Usage:**
```haskell
main = do
  let timePoints = [0.0, 0.5, 1.0, 1.5, 2.0]
  writeSimulationResultsInCSV "results.csv" timePoints model specs
```

## Advanced Output Functions

### **Custom Output Formats**

#### `printResultSourceWithFormatter`
```haskell
printResultSourceWithFormatter :: (ResultSource -> String) -> ResultSource -> IO ()
```
Prints results using a custom formatter function.

**Usage:**
```haskell
customFormatter :: ResultSource -> String
customFormatter source = 
  "Time: " ++ show (sourceId source) ++ ", Value: " ++ show (sourceName source)

main = do
  printResultSourceWithFormatter customFormatter resultSource
```

### **Batch Processing**

#### `processSimulationResults`
```haskell
processSimulationResults :: (Results -> IO a) -> Simulation Results -> Specs -> IO a
```
Processes simulation results with a custom function.

**Usage:**
```haskell
analyzeResults :: Results -> IO ()
analyzeResults results = do
  putStrLn "Analyzing simulation results..."
  -- Custom analysis logic here

main = do
  processSimulationResults analyzeResults model specs
```

## Statistical Analysis Functions

### **Basic Statistics**

#### `calculateMean`
```haskell
calculateMean :: [Double] -> Double
```
Calculates the mean of a list of values.

#### `calculateVariance`
```haskell
calculateVariance :: [Double] -> Double
```
Calculates the variance of a list of values.

#### `calculateStandardDeviation`
```haskell
calculateStandardDeviation :: [Double] -> Double
```
Calculates the standard deviation of a list of values.

### **Statistical Collection**

#### `newSamplingStats`
```haskell
newSamplingStats :: Simulation SamplingStats
```
Creates a new sampling statistics collector.

#### `addSamplingStats`
```haskell
addSamplingStats :: SamplingStats -> Double -> Simulation ()
```
Adds a value to the sampling statistics.

#### `getSamplingStats`
```haskell
getSamplingStats :: SamplingStats -> Simulation SamplingStatsData
```
Retrieves the collected statistics.

**Usage:**
```haskell
model = do
  stats <- newSamplingStats
  
  -- Collect data during simulation
  scheduleEvent 1.0 $ do
    xVal <- x
    addSamplingStats stats xVal
  
  -- Get final statistics
  finalStats <- getSamplingStats stats
  putStrLn $ "Mean: " ++ show (samplingStatsMean finalStats)
```

## Data Export Functions

### **JSON Export**

#### `exportResultsToJSON`
```haskell
exportResultsToJSON :: FilePath -> [Double] -> Simulation Results -> Specs -> IO ()
```
Exports simulation results to JSON format.

**Usage:**
```haskell
main = do
  let timePoints = [0.0, 0.5, 1.0, 1.5, 2.0]
  exportResultsToJSON "results.json" timePoints model specs
```

### **XML Export**

#### `exportResultsToXML`
```haskell
exportResultsToXML :: FilePath -> [Double] -> Simulation Results -> Specs -> IO ()
```
Exports simulation results to XML format.

### **Database Export**

#### `exportResultsToDatabase`
```haskell
exportResultsToDatabase :: ConnectionString -> [Double] -> Simulation Results -> Specs -> IO ()
```
Exports simulation results to a database.

## Real-time Output

### **Live Monitoring**

#### `monitorSimulationResults`
```haskell
monitorSimulationResults :: Double -> (Results -> IO ()) -> Simulation Results -> Specs -> IO ()
```
Monitors simulation results in real-time.

**Usage:**
```haskell
monitorFunction :: Results -> IO ()
monitorFunction results = do
  putStrLn "Current simulation state:"
  -- Process and display current results

main = do
  monitorSimulationResults 0.1 monitorFunction model specs
```

### **Progress Reporting**

#### `reportSimulationProgress`
```haskell
reportSimulationProgress :: Double -> Simulation Results -> Specs -> IO ()
```
Reports simulation progress at regular intervals.

## Error Handling in Output

### **Safe Output Functions**

#### `safePrintSimulationResults`
```haskell
safePrintSimulationResults :: [Double] -> ResultPrinting -> Simulation Results -> Specs -> IO (Either String ())
```
Safely prints simulation results with error handling.

**Usage:**
```haskell
main = do
  result <- safePrintSimulationResults timePoints printResultSourceInEnglish model specs
  case result of
    Left error -> putStrLn $ "Error: " ++ error
    Right _ -> putStrLn "Output completed successfully"
```

### **Output Validation**

#### `validateResults`
```haskell
validateResults :: Results -> Bool
```
Validates that results contain expected data.

## Performance Considerations

### **Output Optimization**

#### **Batch Output**
- Collect multiple results before outputting
- Use buffered I/O for file operations
- Minimize output frequency for large simulations

#### **Memory Management**
- Use lazy evaluation for large result sets
- Stream results to files for very large simulations
- Consider using external databases for massive datasets

### **Output Formats**

#### **Text Output**
- **Pros**: Human-readable, easy to parse
- **Cons**: Large file sizes, slower processing
- **Best for**: Debugging, small datasets

#### **CSV Output**
- **Pros**: Compact, easy to import into spreadsheets
- **Cons**: Limited metadata
- **Best for**: Data analysis, external tools

#### **JSON Output**
- **Pros**: Rich metadata, structured data
- **Cons**: Larger file sizes
- **Best for**: Web applications, complex data

#### **Binary Output**
- **Pros**: Very compact, fast I/O
- **Cons**: Not human-readable
- **Best for**: Large datasets, performance-critical applications

## Best Practices

### **Output Design**
1. **Choose Appropriate Format**: Select format based on intended use
2. **Include Metadata**: Add timestamps, simulation parameters
3. **Validate Output**: Check for reasonable values
4. **Handle Errors**: Provide meaningful error messages
5. **Document Format**: Explain output format to users

### **Performance Optimization**
1. **Minimize I/O**: Batch output operations
2. **Use Appropriate Precision**: Don't output unnecessary decimal places
3. **Stream Large Datasets**: Don't hold everything in memory
4. **Profile Output**: Measure output performance impact
5. **Consider Compression**: Compress large output files

### **Data Management**
1. **Backup Results**: Keep copies of important results
2. **Version Control**: Track changes to output formats
3. **Archive Old Results**: Move old results to long-term storage
4. **Validate Data**: Check for data integrity issues
5. **Document Assumptions**: Note any data processing assumptions

## Example: Complete Output System

```haskell
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Results
import Simulation.Aivika.Results.IO

-- Simulation model
model :: Simulation Results
model = mdo
  let g = 9.81
      k = 0.8
  
  let dv = do
        x' <- x
        v' <- v
        if x' < 0
          then return $ Left (-k * v')
          else return $ Right (-g)
  
  let dx = do
        x' <- x
        v' <- v
        if x' < 0
          then return $ Left 0
          else return $ Right v'
  
  v <- integEither dv 15.0
  x <- integEither dx 10.0
  
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "x" "position" x
    , resultSource "v" "velocity" v
    ]

-- Main function with multiple output formats
main = do
  let specs = Specs 
        { spcStartTime = 0.0
        , spcStopTime = 5.0
        , spcDT = 0.1
        , spcMethod = RungeKutta4
        , spcGeneratorType = SimpleGenerator
        }
  
  let timePoints = [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]
  
  putStrLn "Bouncing Ball Simulation - Multiple Output Formats"
  putStrLn "=================================================="
  
  -- Console output
  putStrLn "\n1. Console Output (English):"
  printSimulationResultsInTimes timePoints printResultSourceInEnglish model specs
  
  -- CSV file output
  putStrLn "\n2. CSV File Output:"
  writeSimulationResultsInCSV "bouncing_ball.csv" timePoints model specs
  putStrLn "Results written to bouncing_ball.csv"
  
  -- JSON file output
  putStrLn "\n3. JSON File Output:"
  exportResultsToJSON "bouncing_ball.json" timePoints model specs
  putStrLn "Results written to bouncing_ball.json"
  
  putStrLn "\nSimulation complete."
```

This comprehensive output system demonstrates how to use the Results modules to collect, format, and export simulation data in various formats for different use cases. 