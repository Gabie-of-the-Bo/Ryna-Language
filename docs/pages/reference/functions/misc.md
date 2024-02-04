<link rel="stylesheet" href="../../../../css/reference.css">

### truncate

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>truncate(n: Int) -> Int</code> </td>
            <td rowspan="1">
                Truncates <code>n</code> to be 64 bits at most, keeping the least significat bits
            </td>
        </tr>
    </tbody>
</table>

### panic

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>panic(err: String) -> ()</code> </td>
            <td rowspan="1">
                Throws <code>err</code> as an error and stops the execution of the program
            </td>
        </tr>
    </tbody>
</table>

### emit

> Only available in macro code execution contexts

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>emit(text: String) -> ()</code> </td>
            <td rowspan="1" style="width: 70%;">
                Sends <code>text</code> to the macro output buffer to parse the whole buffer when the execution context ends
            </td>
        </tr>
    </tbody>
</table>