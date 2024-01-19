Now we are goint to write an interface for serializing generic data to different formats. This is not the only way,
but gives a strong base to begin with.

## Serializable interface

The main things you would want to support are the following:

1. Serialize any kind of data.
2. Support multiple output formats.
3. Do not make any unnecessary work.

One way to do this is to define a `Serializable` interface and a class for each format you want to support:

```
interface Serializable<Serializer> {
    fn serialize(serializer: @'Serializer, obj: Self) -> String;
}

// A class for each format. These might have state
class SerializerFormat1 {}
class SerializerFormat2 {}
class SerializerFormat3 {}
[...]
```

After this, you could implement `Serializable<FormatClass>` for each datat type that you want to be able to serialize:

```
fn serialize(serializer: @SerializerFormat1, obj: Int) -> String {
    [...]
}

implement Serializable<SerializerFormat1> for Int;
```

## JSON

Let's build an example for JSON using a class called `JSONSerializer`:

```
// Serializer class
class JSONSerializer {}

// Functions
fn serialize(serializer: @JSONSerializer, obj: Int) -> String {
    return obj.deref().to_string();
}

fn serialize(serializer: @JSONSerializer, obj: Float) -> String {
    return obj.deref().to_string();
}

fn serialize(serializer: @JSONSerializer, obj: String) -> String {
    return "\"" + obj + "\"";
}

// Implementations
implement Serializable<JSONSerializer> for Int;
implement Serializable<JSONSerializer> for Float;
implement Serializable<JSONSerializer> for String;
```

Now you can make use of what you built to create `Array` serialization:

```
import * from range;

fn<T> serialize(serializer: @JSONSerializer, obj: Array<'T [Serializable<JSONSerializer>]>) -> String {
    let res = "[";

    for i in range(0, obj.len()) {
        res = res + serializer.serialize(*obj[i]);

        if i < obj.len() - 1 {
            res = res + ", ";
        }
    }

    return res + "]";
}

implement<T> Serializable<JSONSerializer> for Array<'T [Serializable<JSONSerializer>]>;
```

This code allows you to use **bounded template substitution** to specify that an `Array` of `'T` is serializable
as long as `'T` is also serializable. Note that We import the range standard library, this is not strictly needed.

Now we only have dictionaries left. This is one way to support them by using custom classes:

```
// Class to serialize
class Test {
    att_a: Int;
    att_b: Array<Float>;
}

// Fucntions
fn<K, V> serialize_map_field(serializer: @JSONSerializer, key: 'K [Serializable<JSONSerializer>], value: 'V [Serializable<JSONSerializer>]) -> String {
    return serializer.serialize(*key) + ": " + serializer.serialize(*value);
}

fn serialize(serializer: @JSONSerializer, obj: Test) -> String {
    let a = serializer.serialize_map_field("att_a", move(obj.att_a()));
    let b = serializer.serialize_map_field("att_b", move(obj.att_b()));

    return "{" + a + ", " + b + "}";
}

// Implementation
implement Serializable<JSONSerializer> for Test;
```

After this you can serialize `Test` instances like this: 

```
let serializer = JSONSerializer();
let a = arr<Float>();
a.push(5.2);
a.push(8.76);
a.push(15.9);

let test = Test(10, move(a));

print(serializer.serialize(move(test))); // Prints {"att_a": 10, "att_b": [5.2, 8.76, 15.9]}
```