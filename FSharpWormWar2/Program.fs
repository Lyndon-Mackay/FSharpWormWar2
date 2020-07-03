module worm
open System

type Hosts = {suspectable:int; infected:int; immune:int}

type Network = {id:char; hosts:Hosts;}

type Link = { network1:char; network2:char; capacity:int }

type Rate = {S:float ; I:float; R:float}

type payload = 
    |Destination of Char*Char*Char
    |Internal of Char

type deliverable = { parcel:payload; quantity:int }
///handles IO as results
type IOWorkFlow() =

    member this.Bind(x, f) = Result.bind f x

    member this.Return(x) =  Ok x

let inputStreamWorkFlow = new IOWorkFlow()
///Natural number
let (|Nat|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) when int > 0 -> Some(int)
   | _ -> None
///Integer greater then 0
let (|ZeroNAT|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) when int >= 0 -> Some(int)
   | _ -> None

let (|Float|_|) str = 
    match Double.TryParse(str) with
    | (true,v)  when v > 0.0 ->  Some(v) 
    | _ -> None
///Checks string for a single character
let (|Char|_|) (str:string) = 
    match str |> String.length with
    | 1 -> Some (str |> Seq.head)
    | _ -> None

let getNaturalNumber () =
    match Console.ReadLine() with
    | Nat v -> Ok v
    | _ -> Error "invalid number, must be greater then 0"

let tryRead() =
    match Console.ReadLine() with
    | null -> Error "not Expecting empty input"
    | v -> Ok v

let tryNetwork (str:string) = 
    match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with 
    | [|Char c; Nat h; ZeroNAT i |] -> Ok {id = c; hosts = {suspectable = h-i ; infected = i; immune = 0} }
    | _ -> Error "Invalid network" 

let tryLink (str:string) = 
   match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
   | [|Char c1; Char c2; Nat cap  |] -> Ok { network1=c1; network2=c2; capacity= cap}
   | _ -> Error "Invalid Link networkID1(char) networkID2(char) capacity(int greater then 0)"

let tryRate (str:string) = 
    match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
    | [|Float spread; Float infected; Float rate |] -> Ok {S = spread; I = infected; R = rate;}
    | _ -> Error "Invalid Rate spread(float) infected(float) rate(float)"

let getNetworks n =

   let rec tryNewnetwork lis = 

       printfn "enter network ID(char) hosts(integer greater then 0 ) infected(integer greater or equal to 0)"

       tryRead()
       |> Result.bind(tryNetwork)
       |> Result.bind(fun x -> if not ( List.exists(fun y -> x.id = y.id) lis) then Ok x else Error "A network with that ID already exists")
       |>function 
       | Ok  newNet -> match (newNet::lis)  with
                        | v when List.length v  = n -> v
                        | v -> tryNewnetwork v
       | Error e ->  printfn "%s" e
                     tryNewnetwork lis
   tryNewnetwork []

let identicalLink link1 link2 =
    ((link1.network1 = link2.network1 && link1.network2 = link2.network2 )||(link1.network1 = link2.network2 && link1.network2 = link2.network1))

let linkHasNetworks networkLis link1  =
    List.exists(fun x -> link1.network1 = x.id) networkLis && List.exists(fun x -> link1.network2 = x.id) networkLis

let getLinks networkLis n =

   let rec trylistofLinks lis = 

       printfn "enter Link networkID1(char) networkID2(char) capacity(int greater then 0)"

       tryRead()
       |> Result.bind(tryLink)
       |> Result.bind(fun x -> match x.network1 = x.network2 with
                               | false -> Ok x 
                               | true -> Error "can't connect to the same network")
       |> Result.bind(fun x -> if not ( List.exists( identicalLink x  ) lis ) 
                                then Ok x
                                else Error "A Link connecting these two networks already exists")
       |> Result.bind(fun x ->  match linkHasNetworks networkLis x with 
                                | true -> Ok x
                                | false -> Error "Both networks need to exist")
       |>function 
       | Ok  newLink-> match (newLink::lis)  with
                        | v when List.length v = n -> v
                        | v-> trylistofLinks v
       | Error e ->  printfn "%s" e
                     trylistofLinks lis
   trylistofLinks []

let getRate () = 
    printfn "Enter rate spread(float) infected(float) rate(float)"
    tryRead() 
    |> Result.bind tryRate

let countLinkedSuspectHosts links network =
    links
    |> List.filter(fun x -> x.network1 = network.id || x.network2 = network.id)
///Generate packet from each infected host to send out to the wild
let generatePacketsonLink network connectedLis = 
    let rnd = System.Random()

    [0 .. network.hosts.infected ] 
    |> List.map( fun _ ->
            rnd.Next( List.length connectedLis ) 
            |> function
            | 0 -> Internal network.id
            | v -> let newdestination = (List.item v connectedLis  )
                   Destination (newdestination.network1,newdestination.network2,network.id)
    )       
///Attempt to infect hosts withn the same network success is determined by the rate
let spreadInternally rate network internalSpread = 
    let rnd = System.Random()
    let newlyInfected =  (float) network.hosts.suspectable * rate.I * rnd.NextDouble() + List.fold(fun accu x -> match x  with 
                                                                                                                 | Internal _ -> accu+1.0
                                                                                                                 | _ -> accu) 0.0 internalSpread 
                                                                                                                 |> int
    {network with hosts ={network.hosts with suspectable = network.hosts.suspectable - newlyInfected ; infected = network.hosts.infected + newlyInfected; } } //Spreading the virus within the hosts
///Sending data over the link this handles the dropping part due to high traffic
let fillLinks packetSize linkLis spreadPath = 
        linkLis
        |> List.map(fun x -> (x,x.capacity/packetSize)) //get the capcity
        |> List.map(fun x -> List.filter(function
                                         | Destination (v1,v2,_) when (fst x).network1 = v1 && (fst x).network2 = v2 -> true
                                         | _ -> false
                                         ) spreadPath 

                             |> List.truncate (snd x) ) //take the capcity 
        |> List.concat
///From the number of each packets sent to network see how many are infected
let calculateExternallyInfectedHosts rate network newNum =
    let rnd = System.Random()
    let newlyInfected = 
        (float)newNum * rate.I * rnd.NextDouble() 
        |> int
        |> min network.hosts.suspectable

    {network with hosts = {network.hosts with  suspectable = network.hosts.suspectable - newlyInfected; infected = network.hosts.infected + newlyInfected;}}
///Receiving virus from external packets                                                
let spreadExternally rate networks packets = 
    networks 
    |> List.map(fun x -> calculateExternallyInfectedHosts rate x (List.length (List.filter (function
                                                                                            | Destination (_,_,v3) when v3 = x.id-> true
                                                                                            | _ -> false
                                                                                            ) packets )))
///Out of bounds recovery and immunisation of network
let immuniseAndRecover rate networks  = 
    let rnd = System.Random()
    networks 
    |> List.map(fun x -> 
       
       let newlyRecovered = 
            (float) x.hosts.infected * rnd.NextDouble() * rate.R 
            |> int   
            |> min x.hosts.infected

       let newlyImmunised = 
            (float) x.hosts.suspectable * rnd.NextDouble() * rate.I 
            |> int 
            |> min x.hosts.suspectable

       {x with hosts = {suspectable = (x.hosts.suspectable - newlyImmunised); infected = x.hosts.infected - newlyRecovered ;immune = x.hosts.immune + newlyRecovered + newlyImmunised}}
    )
///Returns a sequence that simulates one period of time of the viruses spread and demise
let simulate packetSize (networks:Network list) (links:Link List) rate  = 
    Seq.unfold(fun state ->
        let pcktsize,rte,networkLis,linksLis = state
        
        let spreadPath = 
             networkLis
             |> List.map(fun x -> countLinkedSuspectHosts linksLis  x
                                  |> generatePacketsonLink x
                                  )
             |> List.concat
             
        let networksPostInternalSpread =
             networkLis
             |> List.map(fun x -> spreadInternally rte x (List.filter( function
                                                                       |Internal v when v = x.id -> true
                                                                       | _ -> false) spreadPath))
       
        let finalNetwork = fillLinks pcktsize linksLis spreadPath 
                           |> spreadExternally rte networksPostInternalSpread  
                           |> immuniseAndRecover rate  

        Some (networkLis,(pcktsize,rte,finalNetwork,linksLis))
    ) (packetSize, rate, networks, links )

let inputAll() =

    inputStreamWorkFlow
        {
           printfn "Enter Number of networks"
           let! networks = getNaturalNumber()
                           |> Result.map getNetworks

           printfn "Enter Number of Links"
           let! links = getNaturalNumber()
                        |> Result.map (getLinks networks)
                        
           let! rate = getRate()

           printfn "Enter packet size"
           let! packetSize = getNaturalNumber()
           
           return simulate packetSize networks links rate
        }

[<EntryPoint>]
let main argv =
    inputAll()
    |>function
    | Ok result -> result
                   |> Seq.take 600
                   |> Seq.iter ( List.iter( printfn "%A")  )
    | Error e -> printfn "%s" e
    0 // return an integer exit code