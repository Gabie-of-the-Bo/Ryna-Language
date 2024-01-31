<link rel="stylesheet" href="../../../../css/reference.css">

### print

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>print(obj: Int) -> ()</code> </td>
            <td rowspan="4">
                Prints <code>obj</code> to stdout
            </td>
        </tr>
        <tr>
            <td rowspan="1"> <code>print(obj: Float) -> ()</code> </td>
        </tr>
        <tr>
            <td rowspan="1"> <code>print(obj: Bool) -> ()</code> </td>
        </tr>
        <tr>
            <td rowspan="1"> <code>print(obj: Str) -> ()</code> </td>
        </tr>
    </tbody>
</table>

### create_file

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>create_file(path: String) -> File</code> </td>
            <td rowspan="1">
                Create file at <code>path</code> and return it
            </td>
        </tr>
    </tbody>
</table>

### open_file

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>open_file(path: String, r: Bool, w: Bool, a: Bool) -> File</code> </td>
            <td rowspan="1" style="width: 33%;">
                Open file at <code>path</code> with permissions to read (<code>r</code>), write (<code>w</code>) and append (<code>a</code>) and return it
            </td>
        </tr>
    </tbody>
</table>

### remove_file

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>remove_file(path: String) -> Bool</code> </td>
            <td rowspan="1">
                Remove file at <code>path</code> and return <code>true</code> if it succeeded
            </td>
        </tr>
    </tbody>
</table>

### read_str

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>read_str(file: @File) -> String</code> </td>
            <td rowspan="1">
                Read the contents of <code>file</code> as a <code>String</code>
            </td>
        </tr>
    </tbody>
</table>

### read_bytes

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>read_bytes(file: @File) -> Array&lt;Int></code> </td>
            <td rowspan="1">
                Read the contents of <code>file</code> as an <code>Array</code> of bytes
            </td>
        </tr>
        <tr>
            <td rowspan="1"> <code>read_bytes(file: @File, n: Int) -> Array&lt;Int></code> </td>
            <td rowspan="1">
                Read <code>n</code> bytes from <code>file</code>
            </td>
        </tr>
    </tbody>
</table>

### write_str

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>write_str(file: @File, str: &String) -> Bool</code> </td>
            <td rowspan="1" style="width: 50%;">
                Write the contents of <code>str</code> to <code>file</code> and return <code>true</code> if it succeeded
            </td>
        </tr>
    </tbody>
</table>

### write_bytes

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>write_str(file: @File, bytes: &Array&lt;Int>) -> Bool</code> </td>
            <td rowspan="1" style="width: 45%;">
                Write the contents of <code>bytes</code> to <code>file</code> and return <code>true</code> if it succeeded
            </td>
        </tr>
    </tbody>
</table>