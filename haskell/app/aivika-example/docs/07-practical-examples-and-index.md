# Practical Examples and Module Index

## Module Index

### **Core Modules**
- **[01-overview.md](01-overview.md)** - Library overview and key concepts
- **[02-core-simulation.md](02-core-simulation.md)** - Core simulation framework
- **[03-system-dynamics.md](03-system-dynamics.md)** - Differential equation solving
- **[04-results-and-output.md](04-results-and-output.md)** - Results collection and output
- **[05-queue-and-resource-management.md](05-queue-and-resource-management.md)** - Queue and resource management
- **[06-process-and-statistics.md](06-process-and-statistics.md)** - Process modeling and statistics

### **Key Aivika Modules**
- `Simulation.Aivika` - Core simulation framework
- `Simulation.Aivika.SystemDynamics` - Differential equation solvers
- `Simulation.Aivika.Results` - Result collection and management
- `Simulation.Aivika.Results.IO` - Input/output operations
- `Simulation.Aivika.Results.Charting` - Chart generation
- `Simulation.Aivika.Queue` - Queue management
- `Simulation.Aivika.Resource` - Resource allocation
- `Simulation.Aivika.Process` - Process modeling
- `Simulation.Aivika.Statistics` - Statistical analysis

## Practical Examples

### **1. Manufacturing System Simulation**

```haskell
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Queue
import Simulation.Aivika.Resource
import Simulation.Aivika.Results
import Simulation.Aivika.Results.IO

-- Manufacturing system with multiple stages
manufacturingSystem :: Simulation Results
manufacturingSystem = mdo
  -- Resources (machines and workers)
  cuttingMachine <- newResource 2
  assemblyStation <- newResource 3
  qualityInspector <- newResource 1
  packagingStation <- newResource 2
  
  -- Queues for each stage
  rawMaterialQueue <- newFCFSQueue
  cuttingQueue <- newFCFSQueue
  assemblyQueue <- newFCFSQueue
  inspectionQueue <- newFCFSQueue
  packagingQueue <- newFCFSQueue
  
  -- Statistics collectors
  productionStats <- newSamplingStats
  cycleTimeStats <- newTimingStats
  
  -- Production process
  let productionProcess = do
        -- Get raw material
        maybeMaterial <- dequeue rawMaterialQueue
        case maybeMaterial of
          Just materialId -> do
            startTime <- time
            
            -- Stage 1: Cutting
            cuttingId <- requestResource cuttingMachine materialId
            putStrLn $ "Cutting material " ++ show materialId
            scheduleEventWithDelay 1.5 $ do
              releaseResource cuttingId
              enqueue cuttingQueue materialId
              
              -- Stage 2: Assembly
              assemblyId <- requestResource assemblyStation materialId
              putStrLn $ "Assembling material " ++ show materialId
              scheduleEventWithDelay 2.0 $ do
                releaseResource assemblyId
                enqueue assemblyQueue materialId
                
                -- Stage 3: Quality Inspection
                inspectorId <- requestResource qualityInspector materialId
                putStrLn $ "Inspecting material " ++ show materialId
                scheduleEventWithDelay 0.8 $ do
                  releaseResource inspectorId
                  enqueue inspectionQueue materialId
                  
                  -- Stage 4: Packaging
                  packagingId <- requestResource packagingStation materialId
                  putStrLn $ "Packaging material " ++ show materialId
                  scheduleEventWithDelay 1.2 $ do
                    releaseResource packagingId
                    endTime <- time
                    let cycleTime = endTime - startTime
                    addTimingStats cycleTimeStats cycleTime
                    addSamplingStats productionStats 1.0  -- Count completed items
                    putStrLn $ "Material " ++ show materialId ++ " completed (cycle time: " ++ show cycleTime ++ ")"
                    productionProcess  -- Continue production
          
          Nothing -> do
            -- No raw material, wait
            scheduleEventWithDelay 0.1 productionProcess
  
  -- Raw material arrival process
  let materialArrival = do
        materialId <- randomUniform 1 10000
        enqueue rawMaterialQueue materialId
        putStrLn $ "Raw material " ++ show materialId ++ " arrived"
        
        -- Schedule next arrival (exponential distribution)
        nextArrival <- randomExponential 0.8
        scheduleEventWithDelay nextArrival materialArrival
  
  -- Start processes
  scheduleEvent 0.0 materialArrival
  scheduleEvent 0.0 productionProcess
  
  -- Monitor system performance
  scheduleEvent 1.0 $ do
    let monitorPerformance = do
          -- Collect queue statistics
          rawCount <- queueCount rawMaterialQueue
          cuttingCount <- queueCount cuttingQueue
          assemblyCount <- queueCount assemblyQueue
          inspectionCount <- queueCount inspectionQueue
          packagingCount <- queueCount packagingQueue
          
          -- Collect resource utilization
          cuttingUtil <- resourceUtilization cuttingMachine
          assemblyUtil <- resourceUtilization assemblyStation
          inspectorUtil <- resourceUtilization qualityInspector
          packagingUtil <- resourceUtilization packagingStation
          
          putStrLn $ "System Status:"
          putStrLn $ "  Queues: Raw=" ++ show rawCount ++ ", Cutting=" ++ show cuttingCount ++ 
                    ", Assembly=" ++ show assemblyCount ++ ", Inspection=" ++ show inspectionCount ++ 
                    ", Packaging=" ++ show packagingCount
          putStrLn $ "  Utilization: Cutting=" ++ show cuttingUtil ++ ", Assembly=" ++ show assemblyUtil ++ 
                    ", Inspector=" ++ show inspectorUtil ++ ", Packaging=" ++ show packagingUtil
          
          -- Continue monitoring
          scheduleEventWithDelay 5.0 monitorPerformance
    
    monitorPerformance
  
  return $ results 
    [ resultSource "raw_queue" "raw_material_queue" (queueCount rawMaterialQueue)
    , resultSource "cutting_queue" "cutting_queue" (queueCount cuttingQueue)
    , resultSource "assembly_queue" "assembly_queue" (queueCount assemblyQueue)
    , resultSource "inspection_queue" "inspection_queue" (queueCount inspectionQueue)
    , resultSource "packaging_queue" "packaging_queue" (queueCount packagingQueue)
    , resultSource "cutting_util" "cutting_utilization" (resourceUtilization cuttingMachine)
    , resultSource "assembly_util" "assembly_utilization" (resourceUtilization assemblyStation)
    , resultSource "inspector_util" "inspector_utilization" (resourceUtilization qualityInspector)
    , resultSource "packaging_util" "packaging_utilization" (resourceUtilization packagingStation)
    , resultSource "production_rate" "production_rate" (samplingStatsMean <$> getSamplingStats productionStats)
    , resultSource "avg_cycle_time" "average_cycle_time" (timingStatsMean <$> getTimingStats cycleTimeStats)
    ]

-- Main function
main = do
  let specs = Specs 
        { spcStartTime = 0.0
        , spcStopTime = 100.0
        , spcDT = 0.1
        , spcMethod = RungeKutta4
        , spcGeneratorType = SimpleGenerator
        }
  
  putStrLn "Manufacturing System Simulation"
  putStrLn "==============================="
  
  -- Run simulation with periodic output
  printSimulationResultsInIntegTimes 10.0 printResultSourceInEnglish manufacturingSystem specs
  
  putStrLn "\nSimulation complete."
```

### **2. Hospital Emergency Department Simulation**

```haskell
import Simulation.Aivika
import Simulation.Aivika.Queue
import Simulation.Aivika.Resource
import Simulation.Aivika.Statistics
import Simulation.Aivika.Results
import Simulation.Aivika.Results.IO

-- Emergency department simulation
emergencyDepartment :: Simulation Results
emergencyDepartment = mdo
  -- Resources
  triageNurse <- newResource 2
  doctor <- newResource 3
  nurse <- newResource 4
  bed <- newResource 10
  
  -- Queues
  waitingRoom <- newFCFSQueue
  triageQueue <- newFCFSQueue
  treatmentQueue <- newFCFSQueue
  
  -- Statistics
  waitingTimeStats <- newTimingStats
  treatmentTimeStats <- newTimingStats
  patientCountStats <- newSamplingStats
  
  -- Patient arrival process
  let patientArrival = do
        patientId <- randomUniform 1 10000
        arrivalTime <- time
        
        -- Determine patient priority (1=critical, 5=non-urgent)
        priority <- randomUniform 1 5
        let patient = (patientId, priority, arrivalTime)
        
        enqueue waitingRoom patient
        putStrLn $ "Patient " ++ show patientId ++ " arrived (priority: " ++ show priority ++ ")"
        
        -- Schedule next arrival (exponential distribution)
        nextArrival <- randomExponential 3.0
        scheduleEventWithDelay nextArrival patientArrival
  
  -- Triage process
  let triageProcess = do
        maybePatient <- dequeue waitingRoom
        case maybePatient of
          Just (patientId, priority, arrivalTime) -> do
            triageId <- requestResource triageNurse patientId
            putStrLn $ "Triage for patient " ++ show patientId
            
            -- Triage time depends on priority
            triageTime <- randomExponential (6.0 - fromIntegral priority)
            scheduleEventWithDelay triageTime $ do
              releaseResource triageId
              enqueue triageQueue (patientId, priority, arrivalTime)
              putStrLn $ "Triage completed for patient " ++ show patientId
              triageProcess
          
          Nothing -> do
            scheduleEventWithDelay 0.1 triageProcess
  
  -- Treatment process
  let treatmentProcess = do
        maybePatient <- dequeue triageQueue
        case maybePatient of
          Just (patientId, priority, arrivalTime) -> do
            -- Request resources based on priority
            doctorId <- requestResource doctor patientId
            nurseId <- requestResource nurse patientId
            bedId <- requestResource bed patientId
            
            putStrLn $ "Treatment started for patient " ++ show patientId
            
            -- Treatment time depends on priority and complexity
            baseTime <- randomExponential 15.0
            priorityFactor <- return $ 1.0 + (5.0 - fromIntegral priority) * 0.2
            treatmentTime <- return $ baseTime * priorityFactor
            
            scheduleEventWithDelay treatmentTime $ do
              releaseResource doctorId
              releaseResource nurseId
              releaseResource bedId
              
              endTime <- time
              let totalTime = endTime - arrivalTime
              let waitingTime = endTime - arrivalTime - treatmentTime
              
              addTimingStats waitingTimeStats waitingTime
              addTimingStats treatmentTimeStats treatmentTime
              addSamplingStats patientCountStats 1.0
              
              putStrLn $ "Patient " ++ show patientId ++ " discharged (total time: " ++ show totalTime ++ ")"
              treatmentProcess
          
          Nothing -> do
            scheduleEventWithDelay 0.1 treatmentProcess
  
  -- Start processes
  scheduleEvent 0.0 patientArrival
  scheduleEvent 0.0 triageProcess
  scheduleEvent 0.0 treatmentProcess
  
  -- Monitor department performance
  scheduleEvent 1.0 $ do
    let monitorPerformance = do
          waitingCount <- queueCount waitingRoom
          triageCount <- queueCount triageQueue
          treatmentCount <- queueCount treatmentQueue
          
          triageUtil <- resourceUtilization triageNurse
          doctorUtil <- resourceUtilization doctor
          nurseUtil <- resourceUtilization nurse
          bedUtil <- resourceUtilization bed
          
          putStrLn $ "ED Status:"
          putStrLn $ "  Queues: Waiting=" ++ show waitingCount ++ ", Triage=" ++ show triageCount ++ 
                    ", Treatment=" ++ show treatmentCount
          putStrLn $ "  Utilization: Triage=" ++ show triageUtil ++ ", Doctor=" ++ show doctorUtil ++ 
                    ", Nurse=" ++ show nurseUtil ++ ", Beds=" ++ show bedUtil
          
          scheduleEventWithDelay 5.0 monitorPerformance
    
    monitorPerformance
  
  return $ results 
    [ resultSource "waiting_queue" "waiting_room" (queueCount waitingRoom)
    , resultSource "triage_queue" "triage_queue" (queueCount triageQueue)
    , resultSource "treatment_queue" "treatment_queue" (queueCount treatmentQueue)
    , resultSource "triage_util" "triage_utilization" (resourceUtilization triageNurse)
    , resultSource "doctor_util" "doctor_utilization" (resourceUtilization doctor)
    , resultSource "nurse_util" "nurse_utilization" (resourceUtilization nurse)
    , resultSource "bed_util" "bed_utilization" (resourceUtilization bed)
    , resultSource "avg_waiting_time" "average_waiting_time" (timingStatsMean <$> getTimingStats waitingTimeStats)
    , resultSource "avg_treatment_time" "average_treatment_time" (timingStatsMean <$> getTimingStats treatmentTimeStats)
    , resultSource "patients_processed" "patients_processed" (samplingStatsCount <$> getSamplingStats patientCountStats)
    ]

-- Main function
main = do
  let specs = Specs 
        { spcStartTime = 0.0
        , spcStopTime = 200.0
        , spcDT = 0.1
        , spcMethod = RungeKutta4
        , spcGeneratorType = SimpleGenerator
        }
  
  putStrLn "Emergency Department Simulation"
  putStrLn "================================"
  
  printSimulationResultsInIntegTimes 20.0 printResultSourceInEnglish emergencyDepartment specs
  
  putStrLn "\nSimulation complete."
```

### **3. Supply Chain Simulation**

```haskell
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Queue
import Simulation.Aivika.Resource
import Simulation.Aivika.Statistics
import Simulation.Aivika.Results
import Simulation.Aivika.Results.IO

-- Supply chain simulation with inventory management
supplyChain :: Simulation Results
supplyChain = mdo
  -- Inventory levels (continuous variables)
  supplierInventory <- integ dSupplierInventory 1000.0
  warehouseInventory <- integ dWarehouseInventory 500.0
  retailInventory <- integ dRetailInventory 200.0
  
  -- Queues for orders
  supplierOrders <- newFCFSQueue
  warehouseOrders <- newFCFSQueue
  retailOrders <- newFCFSQueue
  
  -- Resources (transportation and processing)
  truck <- newResource 5
  warehouseProcessor <- newResource 3
  retailProcessor <- newResource 2
  
  -- Statistics
  orderFulfillmentStats <- newSamplingStats
  inventoryCostStats <- newSamplingStats
  
  -- Customer demand (continuous)
  let customerDemand = do
        baseDemand <- return 10.0
        seasonalFactor <- return $ 1.0 + 0.3 * sin (time / 30.0)  -- Monthly seasonality
        randomFactor <- randomUniform 0.8 1.2
        return $ baseDemand * seasonalFactor * randomFactor
  
  -- Inventory dynamics
  let dRetailInventory = do
        demand <- customerDemand
        return $ -demand  -- Inventory decreases due to demand
  
  let dWarehouseInventory = do
        retailInv <- retailInventory
        return $ if retailInv < 50.0 then 20.0 else 0.0  -- Replenish when low
  
  let dSupplierInventory = do
        warehouseInv <- warehouseInventory
        return $ if warehouseInv < 100.0 then 50.0 else 0.0  -- Replenish when low
  
  -- Order processing
  let processRetailOrders = do
        maybeOrder <- dequeue retailOrders
        case maybeOrder of
          Just orderId -> do
            processorId <- requestResource retailProcessor orderId
            putStrLn $ "Processing retail order " ++ show orderId
            
            scheduleEventWithDelay 1.0 $ do
              releaseResource processorId
              addSamplingStats orderFulfillmentStats 1.0
              putStrLn $ "Retail order " ++ show orderId ++ " fulfilled"
              processRetailOrders
          
          Nothing -> do
            scheduleEventWithDelay 0.1 processRetailOrders
  
  let processWarehouseOrders = do
        maybeOrder <- dequeue warehouseOrders
        case maybeOrder of
          Just orderId -> do
            processorId <- requestResource warehouseProcessor orderId
            truckId <- requestResource truck orderId
            
            putStrLn $ "Processing warehouse order " ++ show orderId
            
            scheduleEventWithDelay 2.0 $ do
              releaseResource processorId
              releaseResource truckId
              enqueue retailOrders orderId
              putStrLn $ "Warehouse order " ++ show orderId ++ " shipped to retail"
              processWarehouseOrders
          
          Nothing -> do
            scheduleEventWithDelay 0.1 processWarehouseOrders
  
  let processSupplierOrders = do
        maybeOrder <- dequeue supplierOrders
        case maybeOrder of
          Just orderId -> do
            truckId <- requestResource truck orderId
            
            putStrLn $ "Processing supplier order " ++ show orderId
            
            scheduleEventWithDelay 3.0 $ do
              releaseResource truckId
              enqueue warehouseOrders orderId
              putStrLn $ "Supplier order " ++ show orderId ++ " delivered to warehouse"
              processSupplierOrders
          
          Nothing -> do
            scheduleEventWithDelay 0.1 processSupplierOrders
  
  -- Order generation based on inventory levels
  let generateOrders = do
        retailInv <- retailInventory
        warehouseInv <- warehouseInventory
        supplierInv <- supplierInventory
        
        -- Generate orders when inventory is low
        if retailInv < 50.0 then do
          orderId <- randomUniform 1 10000
          enqueue warehouseOrders orderId
          putStrLn $ "Retail order " ++ show orderId ++ " generated (inventory: " ++ show retailInv ++ ")"
        else return ()
        
        if warehouseInv < 100.0 then do
          orderId <- randomUniform 1 10000
          enqueue supplierOrders orderId
          putStrLn $ "Warehouse order " ++ show orderId ++ " generated (inventory: " ++ show warehouseInv ++ ")"
        else return ()
        
        -- Continue monitoring
        scheduleEventWithDelay 1.0 generateOrders
  
  -- Start processes
  scheduleEvent 0.0 processRetailOrders
  scheduleEvent 0.0 processWarehouseOrders
  scheduleEvent 0.0 processSupplierOrders
  scheduleEvent 0.0 generateOrders
  
  -- Monitor supply chain performance
  scheduleEvent 1.0 $ do
    let monitorPerformance = do
          retailInv <- retailInventory
          warehouseInv <- warehouseInventory
          supplierInv <- supplierInventory
          
          retailQueue <- queueCount retailOrders
          warehouseQueue <- queueCount warehouseOrders
          supplierQueue <- queueCount supplierOrders
          
          truckUtil <- resourceUtilization truck
          warehouseUtil <- resourceUtilization warehouseProcessor
          retailUtil <- resourceUtilization retailProcessor
          
          -- Calculate inventory costs
          totalInventory <- return $ retailInv + warehouseInv + supplierInv
          inventoryCost <- return $ totalInventory * 10.0  -- $10 per unit
          addSamplingStats inventoryCostStats inventoryCost
          
          putStrLn $ "Supply Chain Status:"
          putStrLn $ "  Inventory: Retail=" ++ show retailInv ++ ", Warehouse=" ++ show warehouseInv ++ 
                    ", Supplier=" ++ show supplierInv
          putStrLn $ "  Orders: Retail=" ++ show retailQueue ++ ", Warehouse=" ++ show warehouseQueue ++ 
                    ", Supplier=" ++ show supplierQueue
          putStrLn $ "  Utilization: Truck=" ++ show truckUtil ++ ", Warehouse=" ++ show warehouseUtil ++ 
                    ", Retail=" ++ show retailUtil
          putStrLn $ "  Total Inventory Cost: $" ++ show inventoryCost
          
          scheduleEventWithDelay 5.0 monitorPerformance
    
    monitorPerformance
  
  return $ results 
    [ resultSource "retail_inventory" "retail_inventory" retailInventory
    , resultSource "warehouse_inventory" "warehouse_inventory" warehouseInventory
    , resultSource "supplier_inventory" "supplier_inventory" supplierInventory
    , resultSource "retail_orders" "retail_orders" (queueCount retailOrders)
    , resultSource "warehouse_orders" "warehouse_orders" (queueCount warehouseOrders)
    , resultSource "supplier_orders" "supplier_orders" (queueCount supplierOrders)
    , resultSource "truck_utilization" "truck_utilization" (resourceUtilization truck)
    , resultSource "warehouse_utilization" "warehouse_utilization" (resourceUtilization warehouseProcessor)
    , resultSource "retail_utilization" "retail_utilization" (resourceUtilization retailProcessor)
    , resultSource "order_fulfillment_rate" "order_fulfillment_rate" (samplingStatsMean <$> getSamplingStats orderFulfillmentStats)
    , resultSource "avg_inventory_cost" "average_inventory_cost" (samplingStatsMean <$> getSamplingStats inventoryCostStats)
    ]

-- Main function
main = do
  let specs = Specs 
        { spcStartTime = 0.0
        , spcStopTime = 100.0
        , spcDT = 0.1
        , spcMethod = RungeKutta4
        , spcGeneratorType = SimpleGenerator
        }
  
  putStrLn "Supply Chain Simulation"
  putStrLn "======================="
  
  printSimulationResultsInIntegTimes 10.0 printResultSourceInEnglish supplyChain specs
  
  putStrLn "\nSimulation complete."
```

### **4. Financial Market Simulation**

```haskell
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Statistics
import Simulation.Aivika.Results
import Simulation.Aivika.Results.IO

-- Financial market simulation with price dynamics
financialMarket :: Simulation Results
financialMarket = mdo
  -- Market variables
  stockPrice <- integ dStockPrice 100.0
  tradingVolume <- integ dTradingVolume 1000.0
  marketSentiment <- integ dMarketSentiment 0.5
  
  -- Statistics
  priceStats <- newSamplingStats
  volumeStats <- newSamplingStats
  volatilityStats <- newSamplingStats
  
  -- Price dynamics (geometric Brownian motion with sentiment)
  let dStockPrice = do
        price <- stockPrice
        sentiment <- marketSentiment
        volume <- tradingVolume
        
        -- Random walk component
        randomWalk <- randomNormal 0.0 0.02
        
        -- Sentiment-driven component
        sentimentEffect <- return $ (sentiment - 0.5) * 0.1
        
        -- Volume effect
        volumeEffect <- return $ (volume - 1000.0) / 10000.0
        
        -- Mean reversion
        meanReversion <- return $ (100.0 - price) * 0.01
        
        return $ price * (randomWalk + sentimentEffect + volumeEffect + meanReversion)
  
  -- Volume dynamics
  let dTradingVolume = do
        price <- stockPrice
        sentiment <- marketSentiment
        
        -- Volume increases with price volatility and sentiment
        priceChange <- return $ abs (price - 100.0) / 100.0
        sentimentEffect <- return $ (sentiment - 0.5) * 500.0
        
        -- Random volume fluctuations
        randomVolume <- randomNormal 0.0 100.0
        
        return $ priceChange * 1000.0 + sentimentEffect + randomVolume
  
  -- Market sentiment dynamics
  let dMarketSentiment = do
        price <- stockPrice
        volume <- tradingVolume
        
        -- Sentiment follows price trends
        priceEffect <- return $ (price - 100.0) / 1000.0
        
        -- Volume effect on sentiment
        volumeEffect <- return $ (volume - 1000.0) / 10000.0
        
        -- Random sentiment changes
        randomSentiment <- randomNormal 0.0 0.01
        
        -- Mean reversion to neutral sentiment
        meanReversion <- return $ (0.5 - marketSentiment) * 0.1
        
        return $ priceEffect + volumeEffect + randomSentiment + meanReversion
  
  -- Market events
  let marketEvents = do
        -- Random market events
        eventType <- randomUniform 0.0 1.0
        
        if eventType < 0.1 then do
          -- Positive news event
          putStrLn "Market Event: Positive news released"
          scheduleEventWithDelay 0.1 $ do
            currentSentiment <- marketSentiment
            -- Boost sentiment temporarily
            return ()
        else if eventType < 0.2 then do
          -- Negative news event
          putStrLn "Market Event: Negative news released"
          scheduleEventWithDelay 0.1 $ do
            currentSentiment <- marketSentiment
            -- Reduce sentiment temporarily
            return ()
        else return ()
        
        -- Schedule next event
        nextEvent <- randomExponential 10.0
        scheduleEventWithDelay nextEvent marketEvents
  
  -- Data collection
  let collectData = do
        price <- stockPrice
        volume <- tradingVolume
        sentiment <- marketSentiment
        
        addSamplingStats priceStats price
        addSamplingStats volumeStats volume
        addSamplingStats volatilityStats (abs (price - 100.0))
        
        -- Continue collecting data
        scheduleEventWithDelay 0.5 collectData
  
  -- Start processes
  scheduleEvent 0.0 marketEvents
  scheduleEvent 0.0 collectData
  
  -- Market monitoring
  scheduleEvent 1.0 $ do
    let monitorMarket = do
          price <- stockPrice
          volume <- tradingVolume
          sentiment <- marketSentiment
          
          putStrLn $ "Market Status:"
          putStrLn $ "  Stock Price: $" ++ show price
          putStrLn $ "  Trading Volume: " ++ show volume
          putStrLn $ "  Market Sentiment: " ++ show sentiment
          
          -- Calculate volatility
          priceStats <- getSamplingStats priceStats
          let volatility = samplingStatsStdDev priceStats
          putStrLn $ "  Price Volatility: " ++ show volatility
          
          scheduleEventWithDelay 5.0 monitorMarket
    
    monitorMarket
  
  return $ results 
    [ resultSource "stock_price" "stock_price" stockPrice
    , resultSource "trading_volume" "trading_volume" tradingVolume
    , resultSource "market_sentiment" "market_sentiment" marketSentiment
    , resultSource "price_volatility" "price_volatility" (samplingStatsStdDev <$> getSamplingStats priceStats)
    , resultSource "avg_volume" "average_volume" (samplingStatsMean <$> getSamplingStats volumeStats)
    , resultSource "avg_sentiment" "average_sentiment" (samplingStatsMean <$> getSamplingStats volatilityStats)
    ]

-- Main function
main = do
  let specs = Specs 
        { spcStartTime = 0.0
        , spcStopTime = 50.0
        , spcDT = 0.01
        , spcMethod = RungeKutta4
        , spcGeneratorType = SimpleGenerator
        }
  
  putStrLn "Financial Market Simulation"
  putStrLn "==========================="
  
  printSimulationResultsInIntegTimes 5.0 printResultSourceInEnglish financialMarket specs
  
  putStrLn "\nSimulation complete."
```

## Quick Reference

### **Common Patterns**

#### **Basic Simulation Structure**
```haskell
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Results

specs = Specs { spcStartTime = 0.0, spcStopTime = 100.0, spcDT = 0.1, spcMethod = RungeKutta4, spcGeneratorType = SimpleGenerator }

model :: Simulation Results
model = do
  -- Your simulation logic here
  return $ results [resultSource "t" "time" time]

main = runSimulation specs model
```

#### **Differential Equations**
```haskell
x <- integ dx x0  -- Continuous integration
x <- integEither dx x0  -- With discrete events
```

#### **Queue Operations**
```haskell
queue <- newFCFSQueue
enqueue queue item
maybeItem <- dequeue queue
```

#### **Resource Management**
```haskell
resource <- newResource capacity
resourceId <- requestResource resource item
releaseResource resourceId
```

#### **Statistics Collection**
```haskell
stats <- newSamplingStats
addSamplingStats stats value
finalStats <- getSamplingStats stats
```

#### **Output Functions**
```haskell
printSimulationResultsInTimes timePoints printResultSourceInEnglish model specs
writeSimulationResultsInCSV "output.csv" timePoints model specs
```

### **Performance Tips**

1. **Use appropriate integration methods**: RK4 for most cases, Euler for simple systems
2. **Optimize time steps**: Smaller DT for accuracy, larger for speed
3. **Limit event scheduling**: Minimize unnecessary events
4. **Use bounded queues**: Prevent memory issues in large simulations
5. **Sample statistics efficiently**: Don't collect data too frequently
6. **Profile your simulations**: Measure performance bottlenecks

### **Common Use Cases**

- **Business Process Simulation**: Queues, resources, processes
- **Physical System Modeling**: Differential equations, constraints
- **Economic Modeling**: Market dynamics, supply chains
- **Scientific Research**: Population dynamics, chemical reactions
- **Manufacturing**: Production lines, inventory management
- **Healthcare**: Patient flow, resource allocation
- **Transportation**: Traffic flow, logistics
- **Finance**: Market simulation, risk analysis

This comprehensive documentation provides a solid foundation for using Aivika in a wide variety of simulation applications, from simple physical systems to complex business processes. 