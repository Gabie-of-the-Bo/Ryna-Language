<link rel="stylesheet" href="../../../../css/reference.css">

These are the functions that are needed to implement the interface `Iterable<'Iterator, 'Element>`:

### iterator

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>iterator(obj: Self) -> 'Iterator</code> </td>
            <td rowspan="1">
                Creates an iterator of type <code>'Iterator</code> over <code>obj</code>
            </td>
        </tr>
    </tbody>
</table>

### next

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>next(it: @'Iterator) -> 'Element</code> </td>
            <td rowspan="1">
                Returns the next element in the iterator and advances it
            </td>
        </tr>
    </tbody>
</table>

### is_consumed

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>is_consumed(it: @'Iterator) -> Bool</code> </td>
            <td rowspan="1">
                Returns <code>true</code> if the iterator has no more elements to traverse
            </td>
        </tr>
    </tbody>
</table>
