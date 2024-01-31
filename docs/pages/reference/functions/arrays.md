<link rel="stylesheet" href="../../../../css/reference.css">

### arr

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>arr&lt;T>() -> Array<'T></code> </td>
            <td rowspan="1">
                Creates an empty <code>Array<'T></code>
            </td>
        </tr>
    </tbody>
</table>

### reserve

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>reserve&lt;T>(arr: @Array<'T>, size: Int) -> ()</code> </td>
            <td rowspan="1" style="width: 40%;">
                Allocates enough memory in <code>arr</code> to contain <code>size</code> elements
            </td>
        </tr>
    </tbody>
</table>

### len

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>len&lt;T>(arr: @Array<'T>) -> Int</code> </td>
            <td rowspan="1">
                Returns the length of <code>arr</code>
            </td>
        </tr>
    </tbody>
</table>

### push

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>push&lt;T>(arr: @Array<'T>, elem: 'T) -> ()</code> </td>
            <td rowspan="1">
                Appends <code>elem</code> to the end of <code>arr</code>
            </td>
        </tr>
    </tbody>
</table>

### capacity

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>capacity&lt;T>(arr: @Array<'T>) -> Int</code> </td>
            <td rowspan="1">
                Returns the allocated size inside of <code>arr</code>
            </td>
        </tr>
    </tbody>
</table>