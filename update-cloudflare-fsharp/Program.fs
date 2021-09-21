// Learn more about F# at http://fsharp.org

open System
open System.IO
open FSharp.Data
open Argu
//open dotenv.net

[<Literal>]
let zonesResponseSample = """
{
  "success": true,
  "errors": [],
  "messages": [],
  "result": [
    {
      "id": "023e105f4ecef8ad9ca31a8372d0c353",
      "name": "example.com",
      "development_mode": 7200,
      "original_name_servers": [
        "ns1.originaldnshost.com",
        "ns2.originaldnshost.com"
      ],
      "original_registrar": "GoDaddy",
      "original_dnshost": "NameCheap",
      "created_on": "2014-01-01T05:20:00.12345Z",
      "modified_on": "2014-01-01T05:20:00.12345Z",
      "activated_on": "2014-01-02T00:01:00.12345Z",
      "owner": {
        "id": {},
        "email": {},
        "type": "user"
      },
      "account": {
        "id": "01a7362d577a6c3019a474fd6f485823",
        "name": "Demo Account"
      },
      "permissions": [
        "#zone:read",
        "#zone:edit"
      ],
      "plan": {
        "id": "e592fd9519420ba7405e1307bff33214",
        "name": "Pro Plan",
        "price": 20,
        "currency": "USD",
        "frequency": "monthly",
        "legacy_id": "pro",
        "is_subscribed": true,
        "can_subscribe": true
      },
      "plan_pending": {
        "id": "e592fd9519420ba7405e1307bff33214",
        "name": "Pro Plan",
        "price": 20,
        "currency": "USD",
        "frequency": "monthly",
        "legacy_id": "pro",
        "is_subscribed": true,
        "can_subscribe": true
      },
      "status": "active",
      "paused": false,
      "type": "full",
      "name_servers": [
        "tony.ns.cloudflare.com",
        "woz.ns.cloudflare.com"
      ]
    }
  ]
}
"""

[<Literal>]
let responseSample = """
{
  "success": true,
  "errors": [],
  "messages": [],
  "result": [
    {
      "id": "372e67954025e0ba6aaa6d586b9e0b59",
      "type": "A",
      "name": "example.com",
      "content": "198.51.100.4",
      "proxiable": true,
      "proxied": false,
      "ttl": 120,
      "locked": false,
      "zone_id": "023e105f4ecef8ad9ca31a8372d0c353",
      "zone_name": "example.com",
      "created_on": "2014-01-01T05:20:00.12345Z",
      "modified_on": "2014-01-01T05:20:00.12345Z",
      "data": {},
      "meta": {
        "auto_added": true,
        "source": "primary"
      }
    }
  ]
}
"""

type CloudFlareZonesJson = JsonProvider<zonesResponseSample>
type CloudflareResponseJson = JsonProvider<responseSample>

type CloudflareRequestBodyJson = JsonProvider<"""
{
    "type": "A",
    "name": "example.com",
    "content": "127.0.0.1",
    "ttl":120
}
""">

type IPAPIResponseJson = JsonProvider<""" { "ip": "123.123.123.123." } """>

//let envVars = DotEnv.Read();

//let AUTH = envVars.["CLOUDFLARE_AUTH"]
let AUTH = Environment.GetEnvironmentVariable "CLOUDFLARE_AUTH"
//let ZONE_ID = Environment.GetEnvironmentVariable "CLOUDFLARE_ZONEID"
let authHeader = "Bearer " + AUTH

let getApiUrl (domain: string) =
    let baseUrl = "https://api.cloudflare.com/client/v4/zones"
    let response = Http.Request (baseUrl,
                    query=["name", domain],
                    headers=[ HttpRequestHeaders.Authorization authHeader;
                                HttpRequestHeaders.ContentType HttpContentTypes.Json ])
    match response.StatusCode with
    | 200 -> ()
    | _ -> failwith "Request failed."
    let responseBody =
        match response.Body with
        | Text(body) -> body |> CloudFlareZonesJson.Parse
        | _ -> failwith "Invalid JSON."
    let zoneId = responseBody.Result.[0].Id
    String.concat "/" ["https://api.cloudflare.com/client/v4/zones"; zoneId.ToString().Replace("-", ""); "dns_records"]

let buildCacheFilename (recordType : string) (recordName : string) (domain : string) =
    String.concat "." ["/tmp/.cloudflare_dns"; recordType; recordName; domain; "addr"]

let writeCache (recordType : string) (recordName : string) (domain : string) (newIP : string) =
    let filename = buildCacheFilename recordType recordName domain
    File.WriteAllText(filename, newIP)


let getCloudflareRecord (apiUrl: string) (recordType : string) (recordName : string) (domain : string) = 
    let response =
        Http.Request (apiUrl,
                      query=["type", recordType;
                             "name", recordName + "." + domain],
                      headers=[ HttpRequestHeaders.Authorization authHeader ])
    match response.StatusCode with
    | 200 -> ()
    | _ -> failwith "Request failed."
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

let updateRecord (recordType : string) (recordName : string) (domain : string) (newIP : string) (dry : bool) (force : bool) =
    writeCache recordType recordName domain newIP
    let dnsApiUrl = getApiUrl domain
    //let (record_id, ip) = getCloudflareRecord dnsApiUrl recordType recordName domain
    match getCloudflareRecord dnsApiUrl recordType recordName domain with
    | Some(record_id, ip) ->
        if ip = newIP && (not force)
        then printf "DNS record is the same as server IP"
        else
            if dry then
                printf "Dry Run: Update IP to %s" newIP
            else
                let body = CloudflareRequestBodyJson.Root(recordType, name=recordName, content=newIP, ttl=1).JsonValue.ToString()
                let response = Http.Request
                                (dnsApiUrl + "/" + record_id.ToString().Replace("-", ""),
                                 body=TextRequest body,
                                 httpMethod=HttpMethod.Put,
                                 headers=[ HttpRequestHeaders.Authorization authHeader;
                                           HttpRequestHeaders.ContentType HttpContentTypes.Json])
                match response.StatusCode with
                | 200 -> printf "Update successful"
                | _ -> failwith "DNS update failed."
    | None ->
        if dry then
            printf "Dry Run: Create %s record: %s" recordType newIP
        else
            let body = CloudflareRequestBodyJson.Root(recordType, name=recordName, content=newIP, ttl=1).JsonValue.ToString()
            let response = Http.Request
                            (dnsApiUrl,
                             body=TextRequest body,
                             httpMethod=HttpMethod.Post,
                             headers=[ HttpRequestHeaders.Authorization authHeader;
                                       HttpRequestHeaders.ContentType HttpContentTypes.Json])
            match response.StatusCode with
            | 200 -> printf "Update successful"
            | _ -> failwith "DNS update failed."
                

let checkCache (recordType : string) (recordName : string) (domain : string) =
    let cacheFile = buildCacheFilename recordType recordName domain
    match (File.Exists cacheFile) with
    | true -> Some((File.ReadAllText cacheFile).Trim())
    | false -> None

let checkRecord (recordType : string) (recordName : string) (domain : string) (pubRecord : string) (dry : bool) (force : bool) =
    let cache = checkCache recordType recordName domain
    let isValid =
        match recordType with
        | "A" -> pubRecord.Contains '.'
        | "AAAA" -> pubRecord.Contains ':'
        | _ -> failwith "Invalid recordType."
    match isValid with
    | true -> match cache with
              | Some(c) when (not force) && c = pubRecord -> printf "IP address unchanged."
              | _ -> updateRecord recordType recordName domain pubRecord dry force
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

type Exiter() =
    interface IExiter with
        member this.Name = "CLIArguments"
        member this.Exit (msg, _) =
            printf "%s" msg
            exit 1


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "update-godaddy.exe", errorHandler=Exiter())
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

    List.iter
        (fun recType ->
            let api = match recType with
                      | A ->
                            results.GetResult(Api, defaultValue="https://api.ipify.org?format=json")
                      | AAAA ->
                            results.GetResult(Api6, defaultValue="https://api6.ipify.org?format=json")
            let pubRecord = getPublicRecord api
            List.iter
                (fun recordName -> checkRecord (recType.ToString()) recordName domain pubRecord dry force)
                recordNames)
        recordTypes
    
    0 // return an integer exit code
