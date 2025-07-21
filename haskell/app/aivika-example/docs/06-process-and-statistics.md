# Process Modeling and Statistics: Simulation.Aivika.Process & Simulation.Aivika.Statistics

## Overview

The Process and Statistics modules provide advanced tools for modeling complex workflows and analyzing simulation data. The Process module enables the creation of sophisticated process flows, while the Statistics module offers comprehensive statistical analysis capabilities.

## Process Modeling: Simulation.Aivika.Process

### **Process Types**

#### **Basic Process**
```haskell
newProcess :: Simulation (Process a)
```
Creates a basic process for modeling workflows.

**Usage:**
```haskell
model = do
  process <- newProcess
  -- Define process workflow
```

#### **Process with Parameters**
```haskell
newProcessWithParams :: ProcessParams -> Simulation (Process a)
```
Creates a process with specific parameters.

**Process Parameters:**
```haskell
data ProcessParams = ProcessParams
  { processName :: String        -- Process name for identification
  , processPriority :: Double    -- Process priority
  , processTimeout :: Maybe Double  -- Optional timeout
  }
```

### **Process Operations**

#### **Process Execution**

##### `runProcess`
```haskell
runProcess :: Process a -> Simulation a -> Simulation a
```
Executes a process with a given action.

**Usage:**
```haskell
model = do
  process <- newProcess
  
  result <- runProcess process $ do
    putStrLn "Executing process step 1"
    scheduleEventWithDelay 1.0 $ putStrLn "Step 1 complete"
    return "step1_result"
  
  putStrLn $ "Process result: " ++ result
```

##### `runProcessWithTimeout`
```haskell
runProcessWithTimeout :: Process a -> Double -> Simulation a -> Simulation (Maybe a)
```
Executes a process with a timeout.

**Usage:**
```haskell
model = do
  process <- newProcess
  
  maybeResult <- runProcessWithTimeout process 5.0 $ do
    -- Long-running operation
    scheduleEventWithDelay 10.0 $ return "result"
  
  case maybeResult of
    Just result -> putStrLn $ "Process completed: " ++ result
    Nothing -> putStrLn "Process timed out"
```

#### **Process Control**

##### `suspendProcess`
```haskell
suspendProcess :: Process a -> Simulation ()
```
Suspends a running process.

##### `resumeProcess`
```haskell
resumeProcess :: Process a -> Simulation ()
```
Resumes a suspended process.

##### `terminateProcess`
```haskell
terminateProcess :: Process a -> Simulation ()
```
Terminates a process.

### **Process Composition**

#### **Sequential Processes**
```haskell
sequentialProcesses :: Simulation Results
sequentialProcesses = do
  process1 <- newProcess
  process2 <- newProcess
  
  -- Run processes sequentially
  result1 <- runProcess process1 $ do
    putStrLn "Step 1: Data preparation"
    scheduleEventWithDelay 2.0 $ return "prepared_data"
  
  result2 <- runProcess process2 $ do
    putStrLn "Step 2: Data processing"
    putStrLn $ "Using data from step 1: " ++ result1
    scheduleEventWithDelay 3.0 $ return "processed_data"
  
  return $ results 
    [ resultSource "step1_result" "step1_result" (return result1)
    , resultSource "step2_result" "step2_result" (return result2)
    ]
```

#### **Parallel Processes**
```haskell
parallelProcesses :: Simulation Results
parallelProcesses = do
  process1 <- newProcess
  process2 <- newProcess
  
  -- Run processes in parallel
  let parallelExecution = do
        result1 <- runProcess process1 $ do
          putStrLn "Process 1: Task A"
          scheduleEventWithDelay 2.0 $ return "task_a_result"
        
        result2 <- runProcess process2 $ do
          putStrLn "Process 2: Task B"
          scheduleEventWithDelay 1.5 $ return "task_b_result"
        
        return (result1, result2)
  
  (result1, result2) <- parallelExecution
  
  return $ results 
    [ resultSource "parallel_result1" "parallel_result1" (return result1)
    , resultSource "parallel_result2" "parallel_result2" (return result2)
    ]
```

### **Process Synchronization**

#### **Process Barriers**
```haskell
processBarrier :: Simulation Results
processBarrier = do
  barrier <- newProcessBarrier 3  -- Wait for 3 processes
  
  process1 <- newProcess
  process2 <- newProcess
  process3 <- newProcess
  
  -- Process 1
  runProcess process1 $ do
    putStrLn "Process 1: Starting"
    scheduleEventWithDelay 1.0 $ do
      putStrLn "Process 1: Reaching barrier"
      waitProcessBarrier barrier
      putStrLn "Process 1: Past barrier"
  
  -- Process 2
  runProcess process2 $ do
    putStrLn "Process 2: Starting"
    scheduleEventWithDelay 2.0 $ do
      putStrLn "Process 2: Reaching barrier"
      waitProcessBarrier barrier
      putStrLn "Process 2: Past barrier"
  
  -- Process 3
  runProcess process3 $ do
    putStrLn "Process 3: Starting"
    scheduleEventWithDelay 0.5 $ do
      putStrLn "Process 3: Reaching barrier"
      waitProcessBarrier barrier
      putStrLn "Process 3: Past barrier"
  
  return $ results []
```

#### **Process Communication**

##### **Process Channels**
```haskell
processCommunication :: Simulation Results
processCommunication = do
  channel <- newProcessChannel
  
  -- Producer process
  producer <- newProcess
  runProcess producer $ do
    putStrLn "Producer: Starting"
    scheduleEventWithDelay 1.0 $ do
      putStrLn "Producer: Sending data"
      sendProcessChannel channel "produced_data"
  
  -- Consumer process
  consumer <- newProcess
  runProcess consumer $ do
    putStrLn "Consumer: Starting"
    scheduleEventWithDelay 2.0 $ do
      putStrLn "Consumer: Receiving data"
      data <- receiveProcessChannel channel
      putStrLn $ "Consumer: Received: " ++ data
  
  return $ results []
```

## Statistics Module: Simulation.Aivika.Statistics

### **Statistical Collectors**

#### **Sampling Statistics**
```haskell
newSamplingStats :: Simulation SamplingStats
```
Creates a collector for sampling statistics.

**Usage:**
```haskell
model = do
  stats <- newSamplingStats
  
  -- Collect data points
  scheduleEvent 1.0 $ do
    value <- randomNormal 0.0 1.0
    addSamplingStats stats value
  
  -- Get final statistics
  finalStats <- getSamplingStats stats
  putStrLn $ "Mean: " ++ show (samplingStatsMean finalStats)
  putStrLn $ "StdDev: " ++ show (samplingStatsStdDev finalStats)
```

#### **Timing Statistics**
```haskell
newTimingStats :: Simulation TimingStats
```
Creates a collector for timing statistics.

**Usage:**
```haskell
model = do
  timingStats <- newTimingStats
  
  -- Record timing events
  scheduleEvent 1.0 $ do
    startTime <- time
    scheduleEventWithDelay 2.0 $ do
      endTime <- time
      addTimingStats timingStats (endTime - startTime)
  
  -- Get final timing statistics
  finalTiming <- getTimingStats timingStats
  putStrLn $ "Average time: " ++ show (timingStatsMean finalTiming)
```

### **Statistical Functions**

#### **Basic Statistics**

##### `calculateMean`
```haskell
calculateMean :: [Double] -> Double
```
Calculates the arithmetic mean of a list of values.

**Usage:**
```haskell
let values = [1.0, 2.0, 3.0, 4.0, 5.0]
let mean = calculateMean values  -- Result: 3.0
```

##### `calculateVariance`
```haskell
calculateVariance :: [Double] -> Double
```
Calculates the variance of a list of values.

##### `calculateStandardDeviation`
```haskell
calculateStandardDeviation :: [Double] -> Double
```
Calculates the standard deviation of a list of values.

##### `calculateMedian`
```haskell
calculateMedian :: [Double] -> Double
```
Calculates the median of a list of values.

#### **Advanced Statistics**

##### `calculatePercentile`
```haskell
calculatePercentile :: Double -> [Double] -> Double
```
Calculates a specific percentile of a list of values.

**Usage:**
```haskell
let values = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
let p95 = calculatePercentile 0.95 values  -- 95th percentile
```

##### `calculateSkewness`
```haskell
calculateSkewness :: [Double] -> Double
```
Calculates the skewness of a distribution.

##### `calculateKurtosis`
```haskell
calculateKurtosis :: [Double] -> Double
```
Calculates the kurtosis of a distribution.

### **Statistical Analysis Examples**

#### **Queue Performance Analysis**
```haskell
queuePerformanceAnalysis :: Simulation Results
queuePerformanceAnalysis = do
  queue <- newFCFSQueue
  queueLengthStats <- newSamplingStats
  waitingTimeStats <- newTimingStats
  
  -- Customer arrival and service process
  let customerProcess = do
        customerId <- randomUniform 1 1000
        arrivalTime <- time
        
        enqueue queue customerId
        
        -- Monitor queue length
        queueLength <- queueCount queue
        addSamplingStats queueLengthStats (fromIntegral queueLength)
        
        -- Service the customer
        scheduleEventWithDelay 1.0 $ do
          maybeCustomer <- dequeue queue
          case maybeCustomer of
            Just _ -> do
              serviceTime <- time
              let waitingTime = serviceTime - arrivalTime
              addTimingStats waitingTimeStats waitingTime
            Nothing -> return ()
        
        -- Schedule next customer
        nextArrival <- randomExponential 2.0
        scheduleEventWithDelay nextArrival customerProcess
  
  -- Start the process
  scheduleEvent 0.0 customerProcess
  
  -- Collect final statistics
  scheduleEvent 100.0 $ do
    queueStats <- getSamplingStats queueLengthStats
    waitingStats <- getTimingStats waitingTimeStats
    
    putStrLn $ "Queue Performance Analysis:"
    putStrLn $ "  Average queue length: " ++ show (samplingStatsMean queueStats)
    putStrLn $ "  Max queue length: " ++ show (samplingStatsMax queueStats)
    putStrLn $ "  Average waiting time: " ++ show (timingStatsMean waitingStats)
    putStrLn $ "  Max waiting time: " ++ show (timingStatsMax waitingStats)
  
  return $ results 
    [ resultSource "avg_queue_length" "average_queue_length" (samplingStatsMean <$> getSamplingStats queueLengthStats)
    , resultSource "avg_waiting_time" "average_waiting_time" (timingStatsMean <$> getTimingStats waitingTimeStats)
    ]
```

#### **Resource Utilization Analysis**
```haskell
resourceUtilizationAnalysis :: Simulation Results
resourceUtilizationAnalysis = do
  resource <- newResource 3
  utilizationStats <- newSamplingStats
  
  -- Monitor resource utilization
  let monitorUtilization = do
        utilization <- resourceUtilization resource
        addSamplingStats utilizationStats utilization
        
        -- Continue monitoring
        scheduleEventWithDelay 0.1 monitorUtilization
  
  -- Start monitoring
  scheduleEvent 0.0 monitorUtilization
  
  -- Simulate resource usage
  let resourceUsage = do
        resourceId <- requestResource resource "task"
        scheduleEventWithDelay (randomExponential 2.0) $ do
          releaseResource resourceId
          resourceUsage  -- Continue using resources
  
  -- Start resource usage
  replicateM_ 5 $ scheduleEvent 0.0 resourceUsage
  
  -- Report final statistics
  scheduleEvent 50.0 $ do
    finalStats <- getSamplingStats utilizationStats
    putStrLn $ "Resource Utilization Analysis:"
    putStrLn $ "  Average utilization: " ++ show (samplingStatsMean finalStats)
    putStrLn $ "  Peak utilization: " ++ show (samplingStatsMax finalStats)
    putStrLn $ "  Utilization std dev: " ++ show (samplingStatsStdDev finalStats)
  
  return $ results 
    [ resultSource "avg_utilization" "average_utilization" (samplingStatsMean <$> getSamplingStats utilizationStats)
    , resultSource "peak_utilization" "peak_utilization" (samplingStatsMax <$> getSamplingStats utilizationStats)
    ]
```

### **Statistical Confidence Intervals**

#### **Confidence Interval Calculation**
```haskell
calculateConfidenceInterval :: Double -> [Double] -> (Double, Double)
```
Calculates a confidence interval for the mean.

**Usage:**
```haskell
let values = [1.0, 2.0, 3.0, 4.0, 5.0]
let (lower, upper) = calculateConfidenceInterval 0.95 values  -- 95% confidence interval
putStrLn $ "95% CI: (" ++ show lower ++ ", " ++ show upper ++ ")"
```

#### **Sample Size Determination**
```haskell
calculateRequiredSampleSize :: Double -> Double -> Double -> Int
```
Calculates the required sample size for a given confidence level and margin of error.

**Parameters:**
- Confidence level (e.g., 0.95 for 95%)
- Margin of error (e.g., 0.05 for 5%)
- Estimated standard deviation

### **Statistical Testing**

#### **Hypothesis Testing**
```haskell
tTest :: [Double] -> [Double] -> Double
```
Performs a t-test to compare two samples.

#### **Chi-Square Test**
```haskell
chiSquareTest :: [Int] -> [Int] -> Double
```
Performs a chi-square test for goodness of fit.

### **Data Visualization Support**

#### **Histogram Generation**
```haskell
generateHistogram :: [Double] -> Int -> [(Double, Int)]
```
Generates histogram data for visualization.

**Usage:**
```haskell
let values = [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0]
let histogram = generateHistogram values 4
-- Result: [(1.0, 2), (2.0, 2), (3.0, 2), (4.0, 1)]
```

#### **Box Plot Data**
```haskell
generateBoxPlotData :: [Double] -> (Double, Double, Double, Double, Double)
```
Generates data for box plots (min, Q1, median, Q3, max).

## Performance Considerations

### **Process Performance**
- **Process Overhead**: Each process has some overhead
- **Process Limits**: Limit the number of concurrent processes
- **Process Cleanup**: Ensure processes are properly terminated
- **Memory Usage**: Monitor memory usage with many processes

### **Statistics Performance**
- **Sampling Frequency**: Don't sample too frequently
- **Memory Management**: Use streaming for large datasets
- **Statistical Accuracy**: Ensure sufficient sample sizes
- **Computational Cost**: Some statistics are expensive to calculate

## Best Practices

### **Process Design**
1. **Modular Design**: Break complex processes into smaller components
2. **Error Handling**: Implement proper error handling in processes
3. **Resource Management**: Ensure processes release resources
4. **Process Communication**: Use appropriate communication mechanisms
5. **Process Monitoring**: Monitor process performance and health

### **Statistical Analysis**
1. **Sample Size**: Ensure adequate sample sizes for reliable statistics
2. **Data Quality**: Validate data before statistical analysis
3. **Statistical Assumptions**: Verify assumptions for statistical tests
4. **Multiple Metrics**: Collect multiple metrics for comprehensive analysis
5. **Confidence Intervals**: Always report confidence intervals

### **Integration**
1. **Process-Statistics Integration**: Use statistics to monitor process performance
2. **Real-time Analysis**: Implement real-time statistical monitoring
3. **Data Export**: Export statistical data for external analysis
4. **Visualization**: Use statistical data for charts and graphs
5. **Documentation**: Document statistical methods and assumptions

These modules provide powerful tools for modeling complex workflows and analyzing simulation data, enabling sophisticated simulation applications with comprehensive statistical analysis capabilities. 