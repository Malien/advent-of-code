{-# LANGUAGE DataKinds #-}
import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           Text.Regex.PCRE
import           Data.Array
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Data.PQueue.Max as PQ

main = readFile "in" >>= print . process

process inp = timeLeft $ (!! 1000) $ walkTunnels positions graph $ PQ.singleton startingNode
  where rooms = map parseValve $ lines inp
        graph = connections rooms
        startingNode = Traversal {
          node=('A','A'), 
          concurrentFlowRate=0, 
          pressureRelieved=0,
          valvesOpened=Set.fromList [name | Room { name, flowRate } <- rooms, flowRate == 0],
          timeLeft=30,
          path=[]
        }
        positions = Map.fromList [(name, 0) | Room { name } <- rooms]

data Room = Room { 
  name :: (Char, Char), 
  flowRate :: Int, 
  tunnels :: [(Char, Char)] 
  } deriving Show

data Action = Stay | OpenValve | WalkTunnel (Char, Char) deriving (Show, Eq)

data Traversal = Traversal {
  node :: (Char, Char),
  concurrentFlowRate :: Int,
  pressureRelieved :: Int,
  valvesOpened :: Set (Char, Char),
  timeLeft :: Int,
  path :: [Action]
  } deriving (Show, Eq)

instance Ord Traversal where
  compare a b = compare (potentialFlowRate a) (potentialFlowRate b)

potentialFlowRate (Traversal { concurrentFlowRate, pressureRelieved, timeLeft }) 
  = pressureRelieved + concurrentFlowRate * timeLeft 

walkTunnels positions graph queue 
  | PQ.null queue = []
  | potentialFlowRate ticked < (positions Map.! node) = walkTunnels positions graph newQueue
  | timeLeft == 0 = t : walkTunnels positions graph newQueue
  | Map.size graph == Set.size valvesOpened = t { pressureRelieved = potentialFlowRate t, path = Stay : path } : walkTunnels positions graph newQueue
  | not (Set.member node valvesOpened) = walkTunnels (newPositions withOpenedValve) graph $ appendQ (withOpenedValve:neighbors)
  | otherwise = walkTunnels (newPositions t) graph $ appendQ neighbors
  where (t@Traversal { timeLeft, valvesOpened, node, concurrentFlowRate, pressureRelieved, path }, newQueue) = PQ.deleteFindMax queue
  -- where (t@Traversal { timeLeft, valvesOpened, node, concurrentFlowRate, pressureRelieved, path }: newQueue) = traceShow timeLeft queue
        neighbors = map (walkTunnel t) $ tunnels (graph Map.! node)
        withOpenedValve = ticked { 
            node,
            pressureRelieved = pressureRelieved + concurrentFlowRate,
            valvesOpened = Set.insert node valvesOpened,
            path = OpenValve : path
          }
        ticked = t { pressureRelieved = pressureRelieved + concurrentFlowRate, timeLeft = timeLeft - 1 }
        -- appendQ = (queue ++)
        appendQ = foldr PQ.insert newQueue
        newPositions t = Map.insert node (Main.pressureRelieved t + Main.concurrentFlowRate t) positions

walkTunnel (t@Traversal { pressureRelieved, concurrentFlowRate, timeLeft, path }) nextNode = t { 
  node = nextNode,
  pressureRelieved = pressureRelieved + concurrentFlowRate,
  timeLeft = timeLeft - 1,
  path = WalkTunnel nextNode : path
  }

connections rooms = Map.fromList [(name room, room) | room <- rooms] 

parseValve line = Room { name = (a,b), flowRate = read flowRate, tunnels }
  where (a:b:_) = drop 6 line
        flowRate = takeWhile (/=';') $ drop 23 line
        tunnels = parseTunnels $ dropWhile (/=';') line

parseTunnels line | isPrefixOf "; tunnels lead to valves " line = map toPair $ splitOn ", " $ drop 25 line
                  | isPrefixOf "; tunnel leads to valve " line = [toPair $ drop 24 line]

toPair [a,b] = (a,b)

test = 
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
  \Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
  \Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
  \Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
  \Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
  \Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
  \Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
  \Valve HH has flow rate=22; tunnel leads to valve GG\n\
  \Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
  \Valve JJ has flow rate=21; tunnel leads to valve II"

