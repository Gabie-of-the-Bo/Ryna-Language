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

### get_file

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>get_file(path: String) -> File</code> </td>
            <td rowspan="1">
                Gets handle for the file located at <code>path</code>
            </td>
        </tr>
    </tbody>
</table>

### open

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>open(file: @File, r: Bool, w: Bool, a: Bool) -> ()</code> </td>
            <td rowspan="1" style="width: 33%;">
                Opens <code>file</code> with permissions to read (<code>r</code>), write (<code>w</code>) and append (<code>a</code>)
            </td>
        </tr>
    </tbody>
</table>

### close

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>close(file: @File) -> ()</code> </td>
            <td rowspan="1" style="width: 65%;">
                Closes the handle of the file. You can reuse the handle by using the <code>open</code> function
            </td>
        </tr>
    </tbody>
</table>

### exists

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>exists(file: @File) -> Bool</code> </td>
            <td rowspan="1">
                returns <code>true</code> if the file exists in the file system
            </td>
        </tr>
    </tbody>
</table>

### delete

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>delete(file: @File) -> Bool</code> </td>
            <td rowspan="1">
                Remove <code>file</code> and return <code>true</code> if it succeeded. You can reuse the handle by using the <code>open</code> function
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
            <td rowspan="1"> <code>write_bytes(file: @File, bytes: &Array&lt;Int>) -> Bool</code> </td>
            <td rowspan="1" style="width: 40%;">
                Write the contents of <code>bytes</code> to <code>file</code> and return <code>true</code> if it succeeded
            </td>
        </tr>
    </tbody>
</table>

### input

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>input() -> String</code> </td>
            <td rowspan="1">
                Asks for user input on console and returns it
            </td>
        </tr>
    </tbody>
</table>

### num_args

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>num_args() -> Int</code> </td>
            <td rowspan="1">
                Returns the number of arguments received from stdin
            </td>
        </tr>
    </tbody>
</table>

### get_arg

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>get_arg(i: Int) -> String</code> </td>
            <td rowspan="1">
                Returns the <code>i</code>'th argument received from stdin
            </td>
        </tr>
    </tbody>
</table>