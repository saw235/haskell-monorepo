# Queue and Resource Management: Simulation.Aivika.Queue & Simulation.Aivika.Resource

## Overview

The Queue and Resource modules provide essential tools for modeling discrete event systems with queuing behavior and resource constraints. These modules are fundamental for business process simulation, manufacturing systems, service operations, and any system involving waiting lines and limited resources.

## Queue Management: Simulation.Aivika.Queue

### **Queue Types**

#### **FCFS (First-Come-First-Served) Queue**
```haskell
newFCFSQueue :: Simulation (FCFSQueue a)
```
Creates a first-come-first-served queue.

**Usage:**
```haskell
model = do
  queue <- newFCFSQueue
  -- Use queue for processing items in arrival order
```

#### **LCFS (Last-Come-First-Served) Queue**
```haskell
newLCFSQueue :: Simulation (LCFSQueue a)
```
Creates a last-come-first-served queue.

#### **Priority Queue**
```haskell
newPriorityQueue :: Simulation (PriorityQueue a)
```
Creates a priority queue where items are processed by priority.

### **Queue Operations**

#### **Enqueue Operations**

##### `enqueue`
```haskell
enqueue :: FCFSQueue a -> a -> Simulation ()
```
Adds an item to the end of a queue.

**Usage:**
```haskell
model = do
  queue <- newFCFSQueue
  enqueue queue "customer1"
  enqueue queue "customer2"
```

##### `enqueueWithPriority`
```haskell
enqueueWithPriority :: PriorityQueue a -> Double -> a -> Simulation ()
```
Adds an item to a priority queue with a specific priority.

**Usage:**
```haskell
model = do
  queue <- newPriorityQueue
  enqueueWithPriority queue 1.0 "high_priority_customer"
  enqueueWithPriority queue 3.0 "low_priority_customer"
```

#### **Dequeue Operations**

##### `dequeue`
```haskell
dequeue :: FCFSQueue a -> Simulation (Maybe a)
```
Removes and returns the next item from the queue.

**Usage:**
```haskell
model = do
  queue <- newFCFSQueue
  enqueue queue "customer1"
  
  maybeItem <- dequeue queue
  case maybeItem of
    Just item -> putStrLn $ "Processing: " ++ item
    Nothing -> putStrLn "Queue is empty"
```

##### `dequeueWithPriority`
```haskell
dequeueWithPriority :: PriorityQueue a -> Simulation (Maybe (Double, a))
```
Removes and returns the highest priority item.

#### **Queue Information**

##### `queueCount`
```haskell
queueCount :: FCFSQueue a -> Simulation Int
```
Returns the current number of items in the queue.

**Usage:**
```haskell
model = do
  queue <- newFCFSQueue
  enqueue queue "item1"
  enqueue queue "item2"
  
  count <- queueCount queue
  putStrLn $ "Queue length: " ++ show count  -- Output: Queue length: 2
```

##### `queueEmpty`
```haskell
queueEmpty :: FCFSQueue a -> Simulation Bool
```
Checks if the queue is empty.

##### `queueFull`
```haskell
queueFull :: FCFSQueue a -> Simulation Bool
```
Checks if the queue is full (for bounded queues).

### **Advanced Queue Features**

#### **Bounded Queues**
```haskell
newBoundedFCFSQueue :: Int -> Simulation (FCFSQueue a)
```
Creates a queue with a maximum capacity.

**Usage:**
```haskell
model = do
  queue <- newBoundedFCFSQueue 10  -- Maximum 10 items
  
  -- Try to add more than capacity
  replicateM_ 15 $ enqueue queue "item"
  
  count <- queueCount queue
  putStrLn $ "Actual queue length: " ++ show count  -- Will be 10
```

#### **Queue Statistics**
```haskell
newQueueStats :: Simulation QueueStats
```
Creates a queue statistics collector.

**Usage:**
```haskell
model = do
  queue <- newFCFSQueue
  stats <- newQueueStats
  
  -- Monitor queue during simulation
  scheduleEvent 1.0 $ do
    count <- queueCount queue
    addQueueStats stats count
  
  -- Get final statistics
  finalStats <- getQueueStats stats
  putStrLn $ "Average queue length: " ++ show (queueStatsMean finalStats)
```

## Resource Management: Simulation.Aivika.Resource

### **Resource Types**

#### **Basic Resource**
```haskell
newResource :: Int -> Simulation (Resource a)
```
Creates a resource with a specified capacity.

**Usage:**
```haskell
model = do
  server <- newResource 3  -- 3 servers available
  -- Use server for processing requests
```

#### **Resource with Strategy**
```haskell
newResourceWithStrategy :: Int -> ResourceStrategy -> Simulation (Resource a)
```
Creates a resource with a specific allocation strategy.

**Available Strategies:**
- `FCFSStrategy` - First-come-first-served
- `PriorityStrategy` - Priority-based allocation
- `RandomStrategy` - Random allocation

### **Resource Operations**

#### **Requesting Resources**

##### `requestResource`
```haskell
requestResource :: Resource a -> a -> Simulation (ResourceId a)
```
Requests a resource unit and returns a resource ID.

**Usage:**
```haskell
model = do
  server <- newResource 2
  
  -- Request a server
  serverId <- requestResource server "customer1"
  
  -- Use the server
  processRequest serverId
  
  -- Release when done
  releaseResource serverId
```

##### `requestResourceWithPriority`
```haskell
requestResourceWithPriority :: Resource a -> Double -> a -> Simulation (ResourceId a)
```
Requests a resource with a specific priority.

#### **Releasing Resources**

##### `releaseResource`
```haskell
releaseResource :: ResourceId a -> Simulation ()
```
Releases a previously acquired resource.

**Usage:**
```haskell
model = do
  server <- newResource 1
  serverId <- requestResource server "customer1"
  
  -- Process the request
  scheduleEventWithDelay 5.0 $ do
    putStrLn "Processing complete"
    releaseResource serverId
```

#### **Resource Information**

##### `resourceCount`
```haskell
resourceCount :: Resource a -> Simulation Int
```
Returns the number of available resource units.

**Usage:**
```haskell
model = do
  server <- newResource 3
  serverId1 <- requestResource server "customer1"
  serverId2 <- requestResource server "customer2"
  
  available <- resourceCount server
  putStrLn $ "Available servers: " ++ show available  -- Output: Available servers: 1
```

##### `resourceUtilization`
```haskell
resourceUtilization :: Resource a -> Simulation Double
```
Returns the current utilization rate (0.0 to 1.0).

##### `resourceBusy`
```haskell
resourceBusy :: Resource a -> Simulation Bool
```
Checks if all resource units are busy.

### **Advanced Resource Features**

#### **Resource Statistics**
```haskell
newResourceStats :: Simulation ResourceStats
```
Creates a resource statistics collector.

**Usage:**
```haskell
model = do
  server <- newResource 2
  stats <- newResourceStats
  
  -- Monitor resource utilization
  scheduleEvent 1.0 $ do
    utilization <- resourceUtilization server
    addResourceStats stats utilization
  
  -- Get final statistics
  finalStats <- getResourceStats stats
  putStrLn $ "Average utilization: " ++ show (resourceStatsMean finalStats)
```

#### **Resource Pools**
```haskell
newResourcePool :: [Resource a] -> Simulation (ResourcePool a)
```
Creates a pool of resources for load balancing.

**Usage:**
```haskell
model = do
  server1 <- newResource 1
  server2 <- newResource 1
  server3 <- newResource 1
  
  pool <- newResourcePool [server1, server2, server3]
  
  -- Request from pool (load balancing)
  resourceId <- requestResourceFromPool pool "customer1"
```

## Combined Queue-Resource Systems

### **Service System Example**

```haskell
serviceSystem :: Simulation Results
serviceSystem = mdo
  -- Create queue and resource
  queue <- newFCFSQueue
  server <- newResource 2
  
  -- Customer arrival process
  let customerArrival = do
        customerId <- randomUniform 1 1000
        enqueue queue customerId
        putStrLn $ "Customer " ++ show customerId ++ " arrived"
        
        -- Schedule next arrival
        nextArrival <- randomExponential 2.0
        scheduleEventWithDelay nextArrival customerArrival
  
  -- Service process
  let serviceProcess = do
        maybeCustomer <- dequeue queue
        case maybeCustomer of
          Just customerId -> do
            serverId <- requestResource server customerId
            putStrLn $ "Customer " ++ show customerId ++ " being served"
            
            -- Service time
            serviceTime <- randomExponential 3.0
            scheduleEventWithDelay serviceTime $ do
              releaseResource serverId
              putStrLn $ "Customer " ++ show customerId ++ " completed"
              serviceProcess  -- Continue serving
          
          Nothing -> do
            -- No customers, wait and try again
            scheduleEventWithDelay 0.1 serviceProcess
  
  -- Start processes
  scheduleEvent 0.0 customerArrival
  scheduleEvent 0.0 serviceProcess
  
  -- Collect statistics
  queueStats <- newQueueStats
  resourceStats <- newResourceStats
  
  -- Monitor system
  scheduleEvent 1.0 $ do
    queueLength <- queueCount queue
    utilization <- resourceUtilization server
    addQueueStats queueStats queueLength
    addResourceStats resourceStats utilization
  
  return $ results 
    [ resultSource "queue_length" "queue_length" (queueCount queue)
    , resultSource "utilization" "resource_utilization" (resourceUtilization server)
    ]
```

### **Manufacturing System Example**

```haskell
manufacturingSystem :: Simulation Results
manufacturingSystem = mdo
  -- Resources
  machine1 <- newResource 1
  machine2 <- newResource 1
  inspector <- newResource 1
  
  -- Queues
  rawMaterialQueue <- newFCFSQueue
  processingQueue <- newFCFSQueue
  inspectionQueue <- newFCFSQueue
  
  -- Production process
  let productionProcess = do
        -- Get raw material
        maybeMaterial <- dequeue rawMaterialQueue
        case maybeMaterial of
          Just materialId -> do
            -- Process on machine 1
            machine1Id <- requestResource machine1 materialId
            putStrLn $ "Processing material " ++ show materialId ++ " on machine 1"
            
            scheduleEventWithDelay 2.0 $ do
              releaseResource machine1Id
              enqueue processingQueue materialId
              
              -- Process on machine 2
              machine2Id <- requestResource machine2 materialId
              putStrLn $ "Processing material " ++ show materialId ++ " on machine 2"
              
              scheduleEventWithDelay 1.5 $ do
                releaseResource machine2Id
                enqueue inspectionQueue materialId
                
                -- Inspection
                inspectorId <- requestResource inspector materialId
                putStrLn $ "Inspecting material " ++ show materialId
                
                scheduleEventWithDelay 0.5 $ do
                  releaseResource inspectorId
                  putStrLn $ "Material " ++ show materialId ++ " completed"
                  productionProcess  -- Continue production
          
          Nothing -> do
            -- No raw material, wait
            scheduleEventWithDelay 0.1 productionProcess
  
  -- Raw material arrival
  let materialArrival = do
        materialId <- randomUniform 1 1000
        enqueue rawMaterialQueue materialId
        putStrLn $ "Raw material " ++ show materialId ++ " arrived"
        
        nextArrival <- randomExponential 1.0
        scheduleEventWithDelay nextArrival materialArrival
  
  -- Start processes
  scheduleEvent 0.0 materialArrival
  scheduleEvent 0.0 productionProcess
  
  return $ results 
    [ resultSource "raw_queue" "raw_material_queue" (queueCount rawMaterialQueue)
    , resultSource "processing_queue" "processing_queue" (queueCount processingQueue)
    , resultSource "inspection_queue" "inspection_queue" (queueCount inspectionQueue)
    , resultSource "machine1_util" "machine1_utilization" (resourceUtilization machine1)
    , resultSource "machine2_util" "machine2_utilization" (resourceUtilization machine2)
    , resultSource "inspector_util" "inspector_utilization" (resourceUtilization inspector)
    ]
```

## Performance Considerations

### **Queue Performance**
- **Unbounded Queues**: Use for unlimited capacity scenarios
- **Bounded Queues**: Use to prevent memory issues in large simulations
- **Priority Queues**: Use when processing order matters
- **Queue Statistics**: Monitor for performance bottlenecks

### **Resource Performance**
- **Resource Capacity**: Choose appropriate capacity for your system
- **Allocation Strategy**: Select strategy based on fairness requirements
- **Resource Pools**: Use for load balancing across multiple resources
- **Utilization Monitoring**: Track resource efficiency

### **Memory Management**
- **Queue Size Limits**: Set reasonable limits for bounded queues
- **Resource Cleanup**: Always release resources when done
- **Statistics Collection**: Use sampling for large systems
- **Event Scheduling**: Minimize unnecessary events

## Best Practices

### **Queue Design**
1. **Choose Appropriate Type**: FCFS for fairness, LCFS for stack behavior, Priority for importance
2. **Set Capacity Limits**: Use bounded queues to prevent memory issues
3. **Monitor Performance**: Track queue lengths and waiting times
4. **Handle Empty Queues**: Always check for empty queues before dequeuing
5. **Use Statistics**: Collect queue statistics for analysis

### **Resource Management**
1. **Request-Release Pattern**: Always release resources after use
2. **Check Availability**: Verify resource availability before requesting
3. **Use Appropriate Capacity**: Size resources based on expected demand
4. **Monitor Utilization**: Track resource utilization for optimization
5. **Handle Contention**: Implement appropriate waiting strategies

### **System Integration**
1. **Combine Queues and Resources**: Use together for realistic systems
2. **Balance Load**: Distribute work across multiple resources
3. **Handle Failures**: Implement error handling for resource failures
4. **Optimize Flow**: Minimize bottlenecks in queue-resource systems
5. **Validate Models**: Test with various load conditions

## Error Handling

### **Queue Errors**
```haskell
safeDequeue :: FCFSQueue a -> Simulation (Either String a)
safeDequeue queue = do
  maybeItem <- dequeue queue
  case maybeItem of
    Just item -> return $ Right item
    Nothing -> return $ Left "Queue is empty"
```

### **Resource Errors**
```haskell
safeRequestResource :: Resource a -> a -> Simulation (Either String (ResourceId a))
safeRequestResource resource item = do
  available <- resourceCount resource
  if available > 0
    then do
      resourceId <- requestResource resource item
      return $ Right resourceId
    else return $ Left "No resources available"
```

These modules provide the foundation for modeling complex discrete event systems with queuing behavior and resource constraints, enabling realistic simulation of real-world processes. 