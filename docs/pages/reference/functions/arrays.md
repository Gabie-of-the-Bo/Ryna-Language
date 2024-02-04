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

### pop

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>pop&lt;T>(arr: @Array<'T>) -> ()</code> </td>
            <td rowspan="1">
                Remove the last element of <code>arr</code> if it exists
            </td>
        </tr>
    </tbody>
</table>

### insert

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>insert&lt;T>(arr: @Array<'T>, elem: 'T, pos: Int) -> ()</code> </td>
            <td rowspan="1">
                Inserts <code>elem</code> into <code>arr</code> at the position <code>pos</code>
            </td>
        </tr>
    </tbody>
</table>

### remove

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>remove&lt;T>(arr: @Array<'T>, pos: Int) -> ()</code> </td>
            <td rowspan="1">
                Remove <code>pos</code>'th element of <code>arr</code>
            </td>
        </tr>
    </tbody>
</table>

### set

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>set&lt;T>(arr: @Array<'T>, elem: 'T, pos: Int) -> ()</code> </td>
            <td rowspan="1">
                Sets the <code>pos</code>'th element of <code>arr</code> to <code>elem</code>
            </td>
        </tr>
    </tbody>
</table>