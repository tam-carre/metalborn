*Notes:*
- This documentation was automatically generated by Servant.
- Except for manual testing, you are not supposed to concern yourself with how the data is serialized; that's all glue code invisible to the developer under normal circumstances. `Elm.hs` generates Elm functions that automatically encode and decode data, and Servant generates Haskell encoders/parsers with TH-generated FromJSON and ToJSON instances.

## POST /api/character

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[null,null]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[null,"Male"]
```

- Example (`application/json;charset=utf-8`):

```javascript
["Kaladin",null]
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
["Kaladin","Male",[{"Singleborn":{"Misting":"Coinshot"}},{"spikedA":[],"spikedF":[],"medallF":[],"grenade":false}],[{"AllomancyBlock":"**Kaladin** is a **Coinshot**: he can use **Allomantic Steel**. Coinshots use their Steel sight to detect and Push metals away in a straight line. They can use this ability to fly through the air by Pushing metals placed on the ground. Coinshots are among the deadliest Metalborn, capable of enhancing the power of gunfire or use small metallic objects (such as coins) as weapons."}]]
```

## POST /api/randomCharacter

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[null,null,{"metalborn":1,"twinborn":0.8,"fullPower":1.0e-2,"spike":5.0e-2,"medall":5.0e-2,"grenade":0.25}]
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[null,"Male",{"metalborn":1,"twinborn":0.8,"fullPower":1.0e-2,"spike":5.0e-2,"medall":5.0e-2,"grenade":0.25}]
```

- Example (`application/json;charset=utf-8`):

```javascript
["Kaladin",null,{"metalborn":1,"twinborn":0.8,"fullPower":1.0e-2,"spike":5.0e-2,"medall":5.0e-2,"grenade":0.25}]
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
["Kaladin","Male",[{"Singleborn":{"Misting":"Coinshot"}},{"spikedA":[],"spikedF":[],"medallF":[],"grenade":false}],[{"AllomancyBlock":"**Kaladin** is a **Coinshot**: he can use **Allomantic Steel**. Coinshots use their Steel sight to detect and Push metals away in a straight line. They can use this ability to fly through the air by Pushing metals placed on the ground. Coinshots are among the deadliest Metalborn, capable of enhancing the power of gunfire or use small metallic objects (such as coins) as weapons."}]]
```
