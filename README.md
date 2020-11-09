# MockJSONAPI Server
MockJSONAPI is a mock server for a generic JSON API built with Delphi and delphimvcframework. Can be used with any client technology and language. When mockjsonapi server receives a requests it matches the request against the `data.json` that have been configured.
If you are an experienced user, grab the latest [binary release here](https://github.com/danieleteti/mockjsonapi/releases/latest) otherwise, keep reading the docs.

## Getting started
Let's say that you need to develop a REST client (e.g. mobile app, web client, web SPA or a desktop thin client) and you need some endpoints to use. The usual problem in this case is that you need to start to develop the server before you can show something to the end user. MockJSONAPI server soves this problem giving to the developer a bare-bone REST server with the standard CRUD interface. The data are stored into a single JSON file.

The configuration of the server are really trivial. The `data.json` file contains all the data that will be served by the server. This is a sample `data.json` file:

```json
{
	"projects": [
		{
			"_oid": 1,
			"department": "marketing",
			"description": "Falcon Fly"
		},
		{			
			"_oid": 2,
			"department": "R&D",
			"description": "NextGen"
		}
	],
	"departments": [],
	"employee": []
}

```

MockJSONAPI server supports *all* the resources that you need. So you can ask to the API any resourceusing an HTTP `GET`. If that resource exists in the `data.json` file, then will be returned to the client, otherwise an empty array will be returned.

If you try to create a new entity under a specific resource, then that resource will be automaticaly created and stored in the file.
I strongly suggest to play with the server using curl or Postman. Just launch `mockjsonapi.exe` and follow the curl session below.

Here's a typical curl session which uses the `mockjsonapi` server.


Let's ask for a non existent resource.
```
Daniele@DANIELETETI C:\Users\Daniele
$ curl -i -XGET http://localhost:8080/api/customers
HTTP/1.1 200 OK
Connection: keep-alive
Content-Type: application/json; charset=utf-8
Content-Length: 11
Date: Mon, 09 Nov 2020 16:48:02 GMT
Server: DelphiMVCFramework
X-Powered-By: DMVCFramework 3.2.1 (carbon)
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: POST,GET,OPTIONS,PUT,DELETE
Access-Control-Allow-Headers: Content-Type, Accept, jwtusername, jwtpassword, authentication, authorization
Access-Control-Allow-Credentials: true
X-XSS-Protection: 1; mode = block
X-Content-Type-Options: nosniff
X-MOCK-JSON-API: 1.1.0

{"data":[]}
```
As expected, the api returns an empty array in `data` property (not an error, just an empty array).

Now, we try to get a specific entity from the resource, and we expect an error in this case.
```
Daniele@DANIELETETI C:\Users\Daniele
$ curl -i -XGET http://localhost:8080/api/customers/3
HTTP/1.1 404 Not Found
Connection: close
Content-Type: application/json; charset=utf-8
Content-Length: 139
Date: Mon, 09 Nov 2020 16:49:01 GMT
Server: DelphiMVCFramework
X-Powered-By: DMVCFramework 3.2.1 (carbon)
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: POST,GET,OPTIONS,PUT,DELETE
Access-Control-Allow-Headers: Content-Type, Accept, jwtusername, jwtpassword, authentication, authorization
Access-Control-Allow-Credentials: true
X-XSS-Protection: 1; mode = block
X-Content-Type-Options: nosniff
X-MOCK-JSON-API: 1.1.0

{"classname":"","detailedmessage":"","apperrorcode":0,"items":[],"statuscode":404,"reasonstring":"error","message":"Not Found","data":null}
```
Here's the error. All the errors are returned as json object.


Now we create an actual object into the resource. The resource will be created automatically.
```
Daniele@DANIELETETI C:\Users\Daniele
$ curl -i -XPOST http://localhost:8080/api/customers --data "{\"name\":\"Daniele Teti\"}"
HTTP/1.1 201 Created
Connection: keep-alive
Content-Type: application/json; charset=utf-8
Content-Length: 85
Date: Mon, 09 Nov 2020 16:50:13 GMT
Server: DelphiMVCFramework
X-Powered-By: DMVCFramework 3.2.1 (carbon)
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: POST,GET,OPTIONS,PUT,DELETE
Access-Control-Allow-Headers: Content-Type, Accept, jwtusername, jwtpassword, authentication, authorization
Access-Control-Allow-Credentials: true
X-XSS-Protection: 1; mode = block
X-Content-Type-Options: nosniff
X-MOCK-JSON-API: 1.1.0
X-REF: /api/customers/61616787-1F08-4187-ADBA-BCB8CA065E96
Location: /api/customers/61616787-1F08-4187-ADBA-BCB8CA065E96

{"data":{"status":"ok","xref":"/api/customers/61616787-1F08-4187-ADBA-BCB8CA065E96"}}
```
The resource and the entity have been created. The entity is available for `GET` requests at the URL returned by the `X-REF` header. Let's try to retrieve the object just created.

```
Daniele@DANIELETETI C:\Users\Daniele
$ curl -i -XGET http://localhost:8080/api/customers/61616787-1F08-4187-ADBA-BCB8CA065E96
HTTP/1.1 200 OK
Connection: keep-alive
Content-Type: application/json; charset=utf-8
Content-Length: 69
Date: Mon, 09 Nov 2020 16:51:13 GMT
Server: DelphiMVCFramework
X-Powered-By: DMVCFramework 3.2.1 (carbon)
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: POST,GET,OPTIONS,PUT,DELETE
Access-Control-Allow-Headers: Content-Type, Accept, jwtusername, jwtpassword, authentication, authorization
Access-Control-Allow-Credentials: true
X-XSS-Protection: 1; mode = block
X-Content-Type-Options: nosniff
X-MOCK-JSON-API: 1.1.0

{"name":"Daniele Teti","_oid":"61616787-1F08-4187-ADBA-BCB8CA065E96"}
```
Got It! Here's the JSON object just created. As you can see, MockJSONAPI server adds a standard `_oid` property as `ObjectIdentitied` that you can use to retrieve the object.

Let's create another entity.
```
Daniele@DANIELETETI C:\Users\Daniele
$ curl -i -XPOST http://localhost:8080/api/customers --data "{\"name\":\"Bruce Banner\"}"
HTTP/1.1 201 Created
Connection: keep-alive
Content-Type: application/json; charset=utf-8
Content-Length: 85
Date: Mon, 09 Nov 2020 16:51:53 GMT
Server: DelphiMVCFramework
X-Powered-By: DMVCFramework 3.2.1 (carbon)
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: POST,GET,OPTIONS,PUT,DELETE
Access-Control-Allow-Headers: Content-Type, Accept, jwtusername, jwtpassword, authentication, authorization
Access-Control-Allow-Credentials: true
X-XSS-Protection: 1; mode = block
X-Content-Type-Options: nosniff
X-MOCK-JSON-API: 1.1.0
X-REF: /api/customers/4F1EB312-AD4F-4EDA-9F64-4CACAAB798E3
Location: /api/customers/4F1EB312-AD4F-4EDA-9F64-4CACAAB798E3

{"data":{"status":"ok","xref":"/api/customers/4F1EB312-AD4F-4EDA-9F64-4CACAAB798E3"}}
```

To retrieve the full collection you can send a GET request to the resource without parameter, as following.
```
Daniele@DANIELETETI C:\Users\Daniele
$ curl -i -XGET http://localhost:8080/api/customers
HTTP/1.1 200 OK
Connection: keep-alive
Content-Type: application/json; charset=utf-8
Content-Length: 150
Date: Mon, 09 Nov 2020 16:59:49 GMT
Server: DelphiMVCFramework
X-Powered-By: DMVCFramework 3.2.1 (carbon)
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: POST,GET,OPTIONS,PUT,DELETE
Access-Control-Allow-Headers: Content-Type, Accept, jwtusername, jwtpassword, authentication, authorization
Access-Control-Allow-Credentials: true
X-XSS-Protection: 1; mode = block
X-Content-Type-Options: nosniff
X-MOCK-JSON-API: 1.1.0

{"data":[{"name":"Daniele Teti","_oid":"61616787-1F08-4187-ADBA-BCB8CA065E96"},{"name":"Bruce Banner","_oid":"4F1EB312-AD4F-4EDA-9F64-4CACAAB798E3"}]}
```

## Endpoints supported by MockJSONAPI Server

### Gets the entire resource list
```
GET /api/resourcename
```

### Gets the resource with `_oid = 1`
```
GET /api/resourcename/1
```

### Create a new resource from the request body
```
POST /api/resourcename
{...body...}
```

### Updates the resource with `_oid = 1' using the request body
```
PUT /api/resourcename/1
{...body...}
```

### Deletes resource with `_oid = 1`
```
DELETE /api/resourcename/1
```

### Deletes resource `resourcename`

```
DELETE /api/resourcename
```

## How to use it

Using MockJSONAPI Server is really siple. Just run the executable with the `data.json` file in the exe folder (the release already contains a sample `data.json` file). You are productive in seconds starting to use the Mock API. If you want to "load" data in the server storage, just change the `data.json` with your own data.
