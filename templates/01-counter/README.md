# Counter Application

- Simple Haskell frontend/backend.
- Backend:
  - Servant GET/POST routes to view/modify a TVar.
- Frontend:
  - Miso application, interfacing with the backend using servant-jsaddle.

## Running

```
nix build .#"hello-spa:exe:hello-backend" -o result-backend
./result-backend/bin/hello-backend

curl http://localhost:8081/counter
curl -d "1" -H "Content-Type: application/json" http://localhost:8081/counter
curl http://localhost:8081/counter
```
