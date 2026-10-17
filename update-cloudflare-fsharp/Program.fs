// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Net
open FSharp.Data
open Argu
//open dotenv.net

[<Literal>]
let zonesResponseSample = """
{
  "success": true,
  "result": [
    {
      "id": "023e105f4ecef8ad9ca31a8372d0c353"
    }
  ]
}
"""

[<Literal>]
let responseSample = """
{
  "success": true,
  "result": [
    {
      "id": "372e67954025e0ba6aaa6d586b9e0b59",
      "content": "198.51.100.4"
    }
  ]
}
"""

type CloudFlareZonesJson = JsonProvider<zonesResponseSample>
type CloudflareResponseJson = JsonProvider<responseSample>

type IPAPIResponseJson = JsonProvider<""" { "ip": "123.123.123.123" } """>

//let envVars = DotEnv.Read();

//let AUTH = envVars.["CLOUDFLARE_AUTH"]
let AUTH = Environment.GetEnvironmentVariable "CLOUDFLARE_AUTH"
//let ZONE_ID = Environment.GetEnvironmentVariable "CLOUDFLARE_ZONEID"
let authHeader = "Bearer " + AUTH

let sameIp (a: string) (b: string) =
    match IPAddress.TryParse a, IPAddress.TryParse b with
    | (true, x), (true, y) -> x.Equals y
    | _ -> a = b

let getApiUrl (domain: string) =
    let baseUrl = "https://api.cloudflare.com/client/v4/zones"
    let response = Http.Request (baseUrl,
                                                silentHttpErrors = true,
                                                query=["name", domain],
                                                headers=[ HttpRequestHeaders.Authorization authHeader;
                                                            HttpRequestHeaders.ContentType HttpContentTypes.Json ])
    match response.StatusCode with
    | 200 -> ()
    | _ -> failwithf "Request failed for domain %s (%d): %A" domain response.StatusCode response.Body
    let responseBody =
        match response.Body with
        | Text(body) -> body |> CloudFlareZonesJson.Parse
        | _ -> failwith "Invalid JSON."
    match responseBody.Success, responseBody.Result.Length with
    | true, l when l <> 0 ->
        let zoneId = responseBody.Result.[0].Id
        String.concat "/" ["https://api.cloudflare.com/client/v4/zones"; zoneId.ToString("N"); "dns_records"]
    | _ -> failwithf "No zones match %s" domain


let buildCacheFilename (recordType : string) (recordName : string) (domain : string) =
    String.concat "." ["/tmp/.cloudflare_dns"; recordType; recordName; domain; "addr"]

let writeCache (recordType : string) (recordName : string) (domain : string) (newIP : string) =
    let filename = buildCacheFilename recordType recordName domain
    File.WriteAllText(filename, newIP)


let getCloudflareRecord (apiUrl: string) (recordType : string) (recordName : string) (domain : string) =
    let fullName =
        if recordName = "@" then domain else recordName + "." + domain
    let response =
        Http.Request (apiUrl,
                      silentHttpErrors = true,
                      query=["type", recordType;
                             "name", fullName],
                      headers=[ HttpRequestHeaders.Authorization authHeader ])
    match response.StatusCode with
    | 200 -> ()
    | _ -> failwithf "Request failed for domain %s (%d): %A" domain response.StatusCode response.Body
    let responseBody =
        match response.Body with
        | Text(body) -> body |> CloudflareResponseJson.Parse
        | _ -> failwith "Invalid JSON."
    match responseBody.Result.Length with
    | 0 -> None
    | _ -> Some(responseBody.Result.[0].Id, responseBody.Result.[0].Content)

let getPublicRecord (api : string) =
    let response =
        Http.RequestString api |> IPAPIResponseJson.Parse
    response.Ip

let updateCloudflare (url: string) (verb: string) (body: string) =
    let response =
        Http.Request(url, body = TextRequest body, httpMethod = verb,
                     silentHttpErrors = true,
                     headers = [ HttpRequestHeaders.Authorization authHeader
                                 HttpRequestHeaders.ContentType HttpContentTypes.Json ])
    if response.StatusCode <> 200 then
        failwithf "Cloudflare %s failed (%d): %A" verb response.StatusCode response.Body

let updateRecord (recordType : string) (recordName : string) (domain : string) (dnsApiUrl : string) (newIP : string) (dry : bool) (force : bool) =
    let fullName =
        if recordName = "@" then domain else recordName + "." + domain
    let cache() = writeCache recordType recordName domain newIP
    //let (record_id, ip) = getCloudflareRecord dnsApiUrl recordType recordName domain
    match getCloudflareRecord dnsApiUrl recordType recordName domain with
    | Some(_, ip) when sameIp ip newIP && not force ->
        printfn "DNS record is the same as server IP: %s" newIP
        cache()
    | Some(_, _) when dry ->
        printfn "Dry Run: Update %s to %s" fullName newIP
    | Some(id, _) ->
        let body = JsonValue.Record [| "type", JsonValue.String recordType;
                                                "name", JsonValue.String fullName;
                                                "ttl", JsonValue.Number 1M;
                                                "content", JsonValue.String newIP; |] |> string
        updateCloudflare (dnsApiUrl + "/" + id.ToString("N")) "PATCH" body
        printfn "Updated %s" newIP
        cache()
    | None when dry ->
        printfn "Dry Run: Create %s record: %s" recordType newIP
    | None ->
        let body = JsonValue.Record [| "type", JsonValue.String recordType;
                                        "name", JsonValue.String fullName;
                                        "ttl", JsonValue.Number 1M;
                                        "content", JsonValue.String newIP; |] |> string
        updateCloudflare dnsApiUrl "POST" body
        printfn "Created %s record %s" recordType newIP
        cache()


let checkCache (recordType : string) (recordName : string) (domain : string) =
    let cacheFile = buildCacheFilename recordType recordName domain
    match (File.Exists cacheFile) with
    | true -> Some((File.ReadAllText cacheFile).Trim())
    | false -> None

let checkRecord (recordType : string) (recordName : string) (domain : string) (dnsApiUrl: string) (pubRecord : string) (dry : bool) (force : bool) =
    let cache = checkCache recordType recordName domain
    let isValid =
        match IPAddress.TryParse pubRecord, recordType with
        | (true, ip), "A" -> ip.AddressFamily = Sockets.AddressFamily.InterNetwork
        | (true, ip), "AAAA" -> ip.AddressFamily = Sockets.AddressFamily.InterNetworkV6
        | _ -> false
    match isValid with
    | true -> match cache with
              | Some(c) when (not force) && c = pubRecord -> printfn "IP address unchanged."
              | _ -> updateRecord recordType recordName domain dnsApiUrl pubRecord dry force
    | false -> failwith "Invalid IP address."

type RecordType =
    | A
    | AAAA

type CLIArguments =
    | [<AltCommandLine("-4")>] Ipv4
    | [<AltCommandLine("-6")>] Ipv6
    | Api of url:string
    | Api6 of url:string
    | [<Mandatory; AltCommandLine("-d")>] Domain of string
    | [<AltCommandLine("-n"); Mandatory>] Record_Name of string list
    | Force
    | Dry

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Ipv4 -> "Update A record."
            | Ipv6 -> "Update AAAA record."
            | Api _ -> "API to use for detecting IPv4 address."
            | Api6 _ -> "API to use for detecting IPv6 address."
            | Domain _ -> "Domain name to update."
            | Record_Name _ -> "DNS record(s) to update, e.g. www, subdomain, etc."
            | Dry -> "Dry run (does not update remote DNS)"
            | Force -> "Force update even if IP address is unchanged."

[<EntryPoint>]
let main argv =
    try
        if String.IsNullOrWhiteSpace AUTH then failwith "CLOUDFLARE_AUTH not set"
        let parser = ArgumentParser.Create<CLIArguments>(programName = "update-cloudflare-fsharp.exe",
                                                                                        errorHandler=ProcessExiter())
        let results = parser.Parse(argv)
        let recordNames = results.GetResult(Record_Name)
        let domain = results.GetResult(Domain)
        let dry = results.Contains Dry
        let force = results.Contains Force
        let mutable recordTypes = []
        if results.Contains Ipv6
            then recordTypes <- AAAA :: recordTypes
        if results.Contains Ipv4
            then recordTypes <- A :: recordTypes
        if recordTypes.IsEmpty then recordTypes <- [ A ]

        let dnsApiUrl = getApiUrl(domain)

        List.iter
            (fun recType ->
                let api = match recType with
                            | A ->
                                    results.GetResult(Api, defaultValue="https://api.ipify.org?format=json")
                            | AAAA ->
                                    results.GetResult(Api6, defaultValue="https://api6.ipify.org?format=json")
                let pubRecord = getPublicRecord api
                List.iter
                    (fun recordName -> checkRecord (recType.ToString()) recordName domain dnsApiUrl pubRecord dry force)
                    recordNames)
            recordTypes
        0
    with ex ->
        eprintfn "%s" ex.Message;
        1
