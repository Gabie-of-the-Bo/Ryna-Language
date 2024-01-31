<link rel="stylesheet" href="../../../../css/reference.css">

### to_string

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>to_string(n: Int) -> String</code> </td>
            <td rowspan="1">
                Returns <code>n</code> as a <code>String</code>
            </td>
        </tr>
    </tbody>
</table>

### code_point_at

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>code_point_at(str: &String, n: Int) -> Int</code> </td>
            <td rowspan="2">
                Returns the unicode code point in <code>str</code> at the position <code>n</code>
            </td>
        </tr>
        <tr>
            <td rowspan="1"> <code>code_point_at(str: @String, n: Int) -> Int</code> </td>
        </tr>
    </tbody>
</table>

### code_point_to_str

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>code_point_to_str(n: Int) -> String</code> </td>
            <td rowspan="1">
                Returns <code>n</code> as a <code>String</code>, using it as an unicode code point
            </td>
        </tr>
    </tbody>
</table>

### code_point_length

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>code_point_length(n: Int) -> Int</code> </td>
            <td rowspan="1">
                Returns the length in bytes of <code>n</code> when encoded in UTF-8
            </td>
        </tr>
    </tbody>
</table>

### utf8_array

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>utf8_array(str: &String) -> Array&lt;Int></code> </td>
            <td rowspan="1">
                Returns <code>str</code> encoded in UTF-8
            </td>
        </tr>
    </tbody>
</table>

### utf8_to_str

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>utf8_to_str(arr: &Array&lt;Int>) -> String</code> </td>
            <td rowspan="1">
                Returns <code>arr</code> as a <code>String</code>, assuming it is encoded in UTF-8
            </td>
        </tr>
    </tbody>
</table>