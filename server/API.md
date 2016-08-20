## GET /tags

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- When there is no parent tag

```javascript
[{"parent_tag":null,"name":"communication","id":1}]
```

- When there is no parent tag, When there is no parent tag

```javascript
[{"parent_tag":null,"name":"communication","id":1},{"parent_tag":null,"name":"communication","id":1}]
```

- When specifying a parent tag

```javascript
[{"parent_tag":1,"name":"cellphone","id":2}]
```

- When there is no parent tag, When there is no parent tag, When there is no parent tag

```javascript
[{"parent_tag":null,"name":"communication","id":1},{"parent_tag":null,"name":"communication","id":1},{"parent_tag":null,"name":"communication","id":1}]
```

## POST /tags/create

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"parent_tag":null,"name":"communication"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- When there is no parent tag

```javascript
{"parent_tag":null,"name":"communication","id":1}
```

- When specifying a parent tag

```javascript
{"parent_tag":1,"name":"cellphone","id":2}
```

