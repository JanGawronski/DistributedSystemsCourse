using System;
using System.Linq;
using System.Collections.Generic;
using Ice;
using Demo;

class Program
{
    static int Main(string[] args)
    {
        try
        {
            if (args.Length == 0) { Console.WriteLine("Usage: cs_client <dedicated|shared> ... [--endpoint 'tcp -h host -p port']"); return 1; }

            var argsList = new List<string>(args);
            string endpoint = "tcp -h localhost -p 10000";
            for (int i = 0; i + 1 < argsList.Count; ++i)
            {
                if (argsList[i] == "--endpoint") { endpoint = argsList[i + 1]; argsList.RemoveRange(i, 2); break; }
            }

            if (argsList.Count == 0) { Console.Error.WriteLine("No command"); return 1; }

            string[] iceInitArgs = Array.Empty<string>();
            using (var communicator = Ice.Util.initialize(ref iceInitArgs))
            {
                string category = argsList[0];

                if (category == "dedicated")
                {
                    if (argsList.Count < 3) { Console.Error.WriteLine("Usage: dedicated <id> <get|set|ping> [data]"); return 1; }
                    string id = argsList[1];
                    string op = argsList[2];

                    var basePrx = communicator.stringToProxy($"{category}/{id}:{endpoint}");
                    var prx = DedicatedPrxHelper.uncheckedCast(basePrx);
                    if (prx == null) { Console.Error.WriteLine("Proxy cast failed"); return 2; }

                    if (op == "get")
                    {
                        var s = prx.getState();
                        Console.WriteLine($"{s.id}|{s.name}|{s.data}");
                    }
                    else if (op == "set")
                    {
                        string data = argsList.Count >= 4 ? argsList[3] : string.Empty;
                        var st = new ItemState(int.Parse(id), "obj-" + id, data);
                        prx.setState(st);
                        Console.WriteLine("OK");
                    }
                    else if (op == "ping")
                    {
                        Console.WriteLine(prx.ping());
                    }
                    else { Console.Error.WriteLine("Unknown op: " + op); return 1; }
                }
                else if (category == "shared")
                {
                    string op = argsList.Count >= 2 ? argsList[1] : "ping";
                    var basePrx = communicator.stringToProxy($"shared/shared:{endpoint}");
                    var prx = SharedPrxHelper.uncheckedCast(basePrx);
                    if (prx == null) { Console.Error.WriteLine("Proxy cast failed"); return 2; }

                    if (op == "get")
                    {
                        var s = prx.getState();
                        Console.WriteLine($"{s.id}|{s.name}|{s.data}");
                    }
                    else if (op == "set")
                    {
                        if (argsList.Count < 5) { Console.Error.WriteLine("Usage: shared set <id> <name> <data>"); return 1; }
                        int idv = int.Parse(argsList[2]); string name = argsList[3]; string data = argsList[4];
                        var st = new ItemState(idv, name, data);
                        prx.setState(st);
                        Console.WriteLine("OK");
                    }
                    else if (op == "ping")
                    {
                        Console.WriteLine(prx.ping());
                    }
                    else { Console.Error.WriteLine("Unknown op: " + op); return 1; }
                }
                else
                {
                    Console.Error.WriteLine($"Unknown category: {category}"); return 1;
                }
            }

            return 0;
        }
        catch (Ice.LocalException ex)
        {
            Console.Error.WriteLine("Ice exception: " + ex);
            return 2;
        }
        catch (System.Exception ex)
        {
            Console.Error.WriteLine("Exception: " + ex.Message);
            return 3;
        }
    }
}
