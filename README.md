# MockJSONAPI Server

MockJSONAPI is a zero-configuration mock REST server built with Delphi and [DelphiMVCFramework](https://github.com/danieleteti/delphimvcframework). It exposes a full CRUD interface backed by a single JSON file — no database, no setup.

Use it to prototype REST clients (mobile apps, SPAs, desktop thin clients) without waiting for the real backend to be ready.

Grab the latest [binary release here](https://github.com/danieleteti/mockjsonapi/releases/latest) or compile from source.

## Quick Start

1. Place `data.json` in the same folder as `mockjsonapi.exe`
2. Optionally create a `.env` file to set the port (default `8080`):
   ```
   port = 8080
   ```
3. Run `mockjsonapi.exe`
4. The server is ready at `http://localhost:8080`

## Data File

The `data.json` file contains all the resources served by the API. Each top-level key is a resource name, its value is an array of entities:

```json
{
  "customers": [
    {
      "_oid": "1",
      "name": "Daniele Teti",
      "email": "daniele@example.com"
    },
    {
      "_oid": "2",
      "name": "Bruce Banner",
      "email": "bruce@example.com"
    }
  ],
  "products": []
}
```

- `_oid` is the unique identifier for each entity (Object ID).
- When you `POST` a new entity, the server auto-generates an `_oid` (UUID) if not provided.
- If you request a resource that doesn't exist in `data.json`, an empty array is returned (not an error).
- If you `POST` to a resource that doesn't exist, it is created automatically.

## API Endpoints

All endpoints use the base path `/api`.

| Method | URL | Description | Status |
|--------|-----|-------------|--------|
| `GET` | `/api/{resource}` | Get all entities in a resource | `200` |
| `GET` | `/api/{resource}/{oid}` | Get a single entity by `_oid` | `200` / `404` |
| `POST` | `/api/{resource}` | Create a new entity | `201` |
| `PUT` | `/api/{resource}/{oid}` | Update an existing entity | `200` |
| `DELETE` | `/api/{resource}/{oid}` | Delete an entity | `200` |
| `DELETE` | `/api/{resource}` | Delete an entire resource | `200` |

## Response Format

All responses are JSON wrapped in a `data` property:

**GET collection** — returns the array of entities:
```json
{"data": [{"_oid": "1", "name": "Daniele Teti"}, {"_oid": "2", "name": "Bruce Banner"}]}
```

**GET single entity** — returns the entity object:
```json
{"data": {"_oid": "1", "name": "Daniele Teti"}}
```

**POST** — returns status and the reference URL to the created entity:
```json
{"data": {"status": "ok", "xref": "/api/customers/A1B2C3D4-..."}}
```
The `Location` and `X-REF` headers also contain the URL of the new entity.

**PUT / DELETE** — returns status:
```json
{"data": {"status": "ok"}}
```

**Errors** (e.g. entity not found) — returns a `404`:
```json
{"message": "Not Found", "statuscode": 404}
```

## curl Examples

**List all customers:**
```bash
curl http://localhost:8080/api/customers
```

**Get a single customer:**
```bash
curl http://localhost:8080/api/customers/1
```

**Create a customer:**
```bash
curl -X POST http://localhost:8080/api/customers \
  -H "Content-Type: application/json" \
  -d '{"name": "Tony Stark", "email": "tony@stark.com"}'
```

**Update a customer:**
```bash
curl -X PUT http://localhost:8080/api/customers/1 \
  -H "Content-Type: application/json" \
  -d '{"name": "Daniele Teti", "email": "daniele@new.com"}'
```

**Delete a customer:**
```bash
curl -X DELETE http://localhost:8080/api/customers/1
```

## CORS

CORS is enabled by default (`Access-Control-Allow-Origin: *`), so the API can be called directly from any web page or SPA running on a different origin.

## Built With

- [Delphi](https://www.embarcadero.com/products/delphi) — Object Pascal
- [DelphiMVCFramework](https://github.com/danieleteti/delphimvcframework) — Web framework for Delphi

## License

Apache License 2.0 — see [LICENSE](LICENSE) for details.
