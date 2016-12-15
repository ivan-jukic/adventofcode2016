using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BreadthSearch
{
    class C 
    {
        public const int MCHIP = 1;
        public const int GEN = 2;

        public const int PM = 1;
        public const int RU = 2;
        public const int PU = 3;
        public const int SR = 4;
        public const int TM = 5;
        // Part 2 items
        public const int EL = 6;
        public const int DL = 7;
    }

    class Item : IEquatable<Item>
    {
        public int el;
        public int type;
        public int code;
        public String s;

        public bool Equals(Item other)
        {
            return this.el == other.el && this.type == other.type;
        }
    }

    class State
    {
        public int depth;
        public int floor;
        public bool final;
        public Dictionary<int, List<Item>> locations;
        public String encoded;
        public float fitness;
        public bool blockFirstFloor;
    }


    class Program
    {
        public const int maxFloors = 4;
        public const float fitnesSensitivity = 0.023f;

        //public const bool useFitness = false;
        public const bool useFitness = true;

        //public const bool useTypeIndependentEncoding = false;
        public const bool useTypeIndependentEncoding = true;

        // public const bool useFirstFloorBlock = false;
        public const bool useFirstFloorBlock = true;
        

        public static int step;
        public static List<State> states;
        public static List<State> finished;
        public static Dictionary<String, bool> processedDict;


        static void Main(string[] args)
        {
            // Test
            states = new List<State> { getTestInitState() };
            run();
            if (finished.Count > 0)
            {
                Console.WriteLine("The final number of steps for test is {0}", step);
            }
            else
            {
                Console.WriteLine("Could not find solution for the test example");
            }

            // Part 1
            states = new List<State> { getInitState() };
            run();
            if (finished.Count > 0)
            {
                Console.WriteLine("The final number of steps for the first part {0}", step);
            }
            else
            {
                Console.WriteLine("Could not find solution for the first part");
            }

            // Part 2
            states = new List<State> { getPart2InitState() };
            run();
            if (finished.Count > 0)
            {
                Console.WriteLine("The final number of steps for the second part {0}", step);
            }
            else
            {
                Console.WriteLine("Could not find solution for the second part");
            }
        }


        static void run()
        {
            step = 0;
            finished = new List<State> { };
            processedDict = new Dictionary<string, bool>();

            while(finished.Count == 0 && states.Count > 0)
            {
                List<State> newStates = new List<State> { };

                foreach(State state in states) {
                    List<State> iterationStates = getPossibleStates(state);

                    foreach (State s in iterationStates)
                    {
                        if (s.final)
                        {
                            finished.Add(s);
                        }
                        else
                        {
                            newStates.Add(s);
                        }
                    }
                }

                if (newStates.Count > 1 && useFitness)
                {
                    float totalFitness = 0;
                    foreach (State s in newStates)
                    {
                        totalFitness += s.fitness;
                    }

                    float averageFitness = totalFitness / newStates.Count;
                    List<State> filteredStates = new List<State> { };
                    foreach (State s in newStates)
                    {
                        if (s.fitness >= averageFitness - fitnesSensitivity)
                        {
                            filteredStates.Add(s);
                        }
                    }

                    newStates = filteredStates;
                }

                states = newStates;
                step++;

                //Console.WriteLine("({0}, {1}, {2})", step, states.Count, processedDict.Count);
            }
        }

        public static List<State> getPossibleStates(State state) {
            int floor = state.floor;
            List<State> newStates = new List<State> { };
            List<Item> floorItems = state.locations[state.floor];
            List<List<Item>> combinations = getCombinations(floorItems);
            
            foreach (List<Item> combo in combinations)
            {
                List<State> upStates =
                    floor < maxFloors ? takeToFloor(floor + 1, state, combo) : new List<State> { };

                List<State> downStates = new List<State> { };
                if (useFirstFloorBlock)
                {
                    downStates = (!state.blockFirstFloor && floor > 1) || (state.blockFirstFloor && floor > 2) ?
                        takeToFloor(floor - 1, state, combo) : new List<State> { };
                }
                else
                {
                    downStates = floor > 1 ? takeToFloor(floor - 1, state, combo) : new List<State> { };
                }

                List<State> iterationStates = upStates.Concat(downStates).ToList();
                newStates = newStates.Concat(iterationStates).ToList();
            }

            return newStates;
        }


        public static List<State> takeToFloor(int nf, State state, List<Item> combo)
        {
            List<Item> nextFloor;
            List<Item> currentFloor;

            if (state.locations.TryGetValue(nf, out nextFloor))
            {
                nextFloor = copyItems(nextFloor);
                nextFloor = nextFloor.Concat(combo).ToList();
            }

            if (state.locations.TryGetValue(state.floor, out currentFloor))
            {
                currentFloor = copyItems(currentFloor);
                currentFloor.RemoveAll(i => combo.Contains(i));
            }

            if (isValidFloor(nextFloor) && isValidFloor(currentFloor))
            {
                Dictionary<int, List<Item>> newLocations = new Dictionary<int, List<Item>>();
                foreach (KeyValuePair<int, List<Item>> pair in state.locations)
                {
                    if (pair.Key == nf)
                    {
                        newLocations.Add(nf, nextFloor);
                    } 
                    else if (pair.Key == state.floor)
                    {
                        newLocations.Add(state.floor, currentFloor);
                    }
                    else
                    {
                        newLocations.Add(pair.Key, copyItems(pair.Value));
                    }
                }

                String newEncode = encodeState(nf, newLocations);

                if (!processedDict.ContainsKey(newEncode))
                {
                    processedDict.Add(newEncode, true);

                    return new List<State>
                    {
                        new State
                        {
                            depth = state.depth + 1,
                            floor = nf,
                            final = isFinal(newLocations),
                            locations = newLocations,
                            encoded = newEncode,
                            fitness = getFitness(newLocations),
                            blockFirstFloor = state.blockFirstFloor ?
                                state.blockFirstFloor : blockFirstFloor(newLocations)
                        }
                    };
                }
            }

            return new List<State> { };
        }


        public static float getFitness(Dictionary<int, List<Item>> locations)
        {
            float total = 0;
            float fitness = 0;
            foreach(KeyValuePair<int, List<Item>> pair in locations)
            {
                total += pair.Value.Count;
                fitness += pair.Key * pair.Value.Count;
            }
            total *= locations.Count;
            return fitness/total;
        }


        public static bool blockFirstFloor(Dictionary<int, List<Item>> locations)
        {
            List<Item> items;

            bool isFirstFloorEmpty = false;
            bool isSecondFloorEmpty = false;

            if (locations.TryGetValue(1, out items))
            {
                isFirstFloorEmpty = items.Count == 0;
            }

            if (locations.TryGetValue(2, out items))
            {
                isSecondFloorEmpty = items.Count == 0;
            }

            return isFirstFloorEmpty && isSecondFloorEmpty;
        }


        public static bool isValidFloor(List<Item> items)
        {
            bool isValid = true;
            if (hasGenerator(items))
            {
                foreach (Item i in items)
                {
                    if (i.type == C.MCHIP && !hasGenerator(i.el, items))
                    {
                        return false;
                    }
                }
            }
            return isValid;
        }
        

        public static bool hasGenerator(int el, List<Item> items)
        {
            foreach (Item i in items)
            {
                if (i.type == C.GEN && i.el == el)
                {
                    return true;
                }
            }
            return false;
        }

        
        public static bool hasGenerator(List<Item> items)
        {
            foreach (Item i in items)
            {
                if (i.type == C.GEN)
                {
                    return true;
                }
            }
            return false;
        }


        public static bool isFinal(Dictionary<int, List<Item>> locations)
        {
            bool final = true;
            foreach(KeyValuePair<int, List<Item>> pair in locations)
            {
                if (pair.Key < maxFloors)
                {
                    final = final && pair.Value.Count == 0;
                }
            }
            return final;
        }


        public static List<List<Item>> getCombinations(List<Item> items)
        {
            List<int> addedCombos = new List<int> { };
            List<List<Item>> combos = new List<List<Item>>{};

            foreach (Item a in items)
            {
                foreach (Item b in items)
                {
                    if (a == b)
                    {
                        continue;
                    }

                    int val = a.code | b.code;
                    if (!addedCombos.Contains(val))
                    {
                        addedCombos.Add(val);
                        combos.Add(new List<Item> { a, b });
                    }
                }
            }

            foreach (Item a in items)
            {
                if (!addedCombos.Contains(a.code))
                {
                    addedCombos.Add(a.code);
                    combos.Add(new List<Item> { a });
                }
            }

            return combos;
        }


        public static String encodeState(int f, Dictionary<int, List<Item>> d)
        {
            if (useTypeIndependentEncoding)
            {
                Dictionary<int, List<String>> itemPairsDict = new Dictionary<int,List<String>>();

                foreach (KeyValuePair<int, List<Item>> pair in d)
                {
                    foreach (Item i in pair.Value)
                    {
                        if (!itemPairsDict.ContainsKey(i.el))
                        {
                            itemPairsDict.Add(i.el, new List<String>() { });
                        }

                        itemPairsDict[i.el].Add(pair.Key.ToString());
                    }
                }

                List<List<String>> itemPairsList = itemPairsDict.Select(kp => kp.Value).ToList();

                List<String> itemPairs = itemPairsList.Select(i => string.Join("_", i.ToArray())).ToList();

                String code = string.Join(";", itemPairs.ToArray());

                return f + ">" + code;
            }
            else
            {
                List<String> code = new List<String> { };
                foreach(KeyValuePair<int, List<Item>> pair in d)
                {
                    int finalCode = 0;
                    foreach (Item i in pair.Value)
                    {
                        finalCode |= i.code;
                    }
                    code.Add(pair.Key.ToString() + ":" + finalCode.ToString());
                }

                return f.ToString() + ">" + String.Join("_", code);
            }
        }


        public static State getTestInitState()
        {
            Dictionary<int, List<Item>> d = new Dictionary<int, List<Item>>
            {
                { 
                    1, new List<Item>
                        { new Item { el = C.PU, type = C.MCHIP, code = 1, s = "PUM" },
                            new Item { el = C.TM, type = C.MCHIP, code = 2, s = "TMM" } }
                },
                {
                    2, new List<Item>
                        { new Item { el = C.TM, type = C.GEN, code = 4, s = "TMG" } }
                },
                { 
                    3, new List<Item>
                        { new Item { el = C.PU, type = C.GEN, code = 8, s = "PUG" } }
                },
                { 
                    4, new List<Item> {}
                },
            };

            return new State
            {
                depth = 0,
                floor = 1,
                final = false,
                encoded = encodeState(1, d),
                locations = d,
                fitness = 0,
                blockFirstFloor = false
            };
        }


        public static State getInitState()
        {
            Dictionary<int, List<Item>> d = new Dictionary<int, List<Item>>
            {
                { 
                    1, new List<Item>
                        { 
                            new Item { el = C.TM, type = C.GEN, code = 1, s = "TMG" },
                            new Item { el = C.TM, type = C.MCHIP, code = 2, s = "TMM" },
                            new Item { el = C.PU, type = C.GEN, code = 4, s = "PUG" },
                            new Item { el = C.SR, type = C.GEN, code = 8, s = "SRG" }
                        }
                },
                {
                    2, new List<Item>
                        {
                            new Item { el = C.PU, type = C.MCHIP, code = 16, s = "PUM" },
                            new Item { el = C.SR, type = C.MCHIP, code = 32, s = "SRM" }
                        }
                },
                { 
                    3, new List<Item>
                        { 
                            new Item { el = C.PM, type = C.GEN, code = 64, s = "PMG" },
                            new Item { el = C.PM, type = C.MCHIP, code = 128, s = "PMM" },
                            new Item { el = C.RU, type = C.GEN, code = 256, s = "RUG" },
                            new Item { el = C.RU, type = C.MCHIP, code = 512, s = "RUM" }
                        }
                },
                { 
                    4, new List<Item> {}
                },
            };

            return new State
            {
                depth = 0,
                floor = 1,
                final = false,
                encoded = encodeState(1, d),
                locations = d,
                fitness = 0,
                blockFirstFloor = false
            };
        }


        public static State getPart2InitState()
        {
            Dictionary<int, List<Item>> d = new Dictionary<int, List<Item>>
            {
                { 
                    1, new List<Item>
                        { 
                            new Item { el = C.TM, type = C.GEN, code = 1, s = "TMG" },
                            new Item { el = C.TM, type = C.MCHIP, code = 2, s = "TMM" },
                            new Item { el = C.PU, type = C.GEN, code = 4, s = "PUG" },
                            new Item { el = C.SR, type = C.GEN, code = 8, s = "SRG" },
                            // Part 2 addition
                            new Item { el = C.EL, type = C.GEN, code = 16, s = "ELG" },
                            new Item { el = C.EL, type = C.MCHIP, code = 32, s = "ELM" },
                            new Item { el = C.DL, type = C.GEN, code = 64, s = "DLG" },
                            new Item { el = C.DL, type = C.MCHIP, code = 128, s = "DLM" },
                        }
                },
                {
                    2, new List<Item>
                        {
                            new Item { el = C.PU, type = C.MCHIP, code = 256, s = "PUM" },
                            new Item { el = C.SR, type = C.MCHIP, code = 512, s = "SRM" }
                        }
                },
                { 
                    3, new List<Item>
                        { 
                            new Item { el = C.PM, type = C.GEN, code = 1024, s = "PMG" },
                            new Item { el = C.PM, type = C.MCHIP, code = 2048, s = "PMM" },
                            new Item { el = C.RU, type = C.GEN, code = 4096, s = "RUG" },
                            new Item { el = C.RU, type = C.MCHIP, code = 8192, s = "RUM" }
                        }
                },
                { 
                    4, new List<Item> {}
                },
            };

            return new State
            {
                depth = 0,
                floor = 1,
                final = false,
                encoded = encodeState(1, d),
                locations = d,
                fitness = 0,
                blockFirstFloor = false
            };
        }


        public static List<Item> copyItems(List<Item> src)
        {
            return src.Select(i => new Item { el = i.el, code = i.code, type = i.type, s = i.s }).ToList();
        }
    }
}
