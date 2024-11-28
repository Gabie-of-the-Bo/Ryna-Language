<link rel="stylesheet" href="../../../../css/reference.css">

### is

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>is&lt;T>(obj: *) -> Bool</code> </td>
            <td rowspan="1">
                Returns <code>true</code> if <code>obj</code> is bindable to the type <code>T</code> 
            </td>
        </tr>
    </tbody>
</table>

### as

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>as&lt;T>(obj: *) -> 'T</code> </td>
            <td rowspan="1" style="width: 60%;">
                Coerces <code>obj</code> to the type <code>T</code>. This does not transform the underlying data,
                only dynamically checks if it is safe to "trick" the type system. This is useful when dealing with
                the different variants of sum types.
            </td>
        </tr>
    </tbody>
</table>

### drop

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>drop&lt;T>(obj: @'T) -> ()</code> </td>
            <td rowspan="1" style="width: 60%;">
                Deletes the contents to which <code>obj</code> is pointing. You cannot access the data after this
                and every other reference pointing to that data becomes invalid
            </td>
        </tr>
    </tbody>
</table>

### move

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>move&lt;T>(obj: @'T) -> 'T</code> </td>
            <td rowspan="1" style="width: 60%;">
                Moves the contents to which <code>obj</code> is pointing to a fresh object of type <code>'T</code>. You cannot access 
                the data after this from any reference to the previous location
            </td>
        </tr>
    </tbody>
</table>

### swap

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>swap&lt;T>(a: @'T, b: @'T) -> ()</code> </td>
            <td rowspan="1" style="width: 60%;">
                Interchanges the contents to which <code>a</code> and <code>b</code> are pointing.
            </td>
        </tr>
    </tbody>
</table>

### ref

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>ref&lt;T>(obj: 'T) -> &'T</code> </td>
            <td rowspan="1">
                Creates a reference to <code>obj</code>
            </td>
        </tr>
    </tbody>
</table>

### mut

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>mut&lt;T>(obj: 'T) -> @'T</code> </td>
            <td rowspan="1">
                Creates a mutable reference to <code>obj</code>
            </td>
        </tr>
    </tbody>
</table>

### deref

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>mut&lt;T>(obj: &'T) -> 'T</code> </td>
            <td rowspan="2">
                Copies the contents to which <code>obj</code> is pointing
            </td>
        </tr>
        <tr>
            <td rowspan="1"> <code>mut&lt;T>(obj: @'T) -> 'T</code> </td>
        </tr>
    </tbody>
</table>

### demut

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>mut&lt;T>(obj: @'T) -> &'T</code> </td>
            <td rowspan="1">
                Creates a reference to <code>obj</code> from a mutable reference
            </td>
        </tr>
    </tbody>
</table>

### fwd

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>fwd&lt;T>(obj: *) -> 'T</code> </td>
            <td rowspan="1" style="width: 60%;">
                Takes <code>obj</code> and tries to convert it to the type <code>T</code>:  
                <ul>
                    <li>If the data's type and <code>T</code> are the same, then the object is returned as is</li>
                    <li>If the object is a mutable reference and <code>T</code> is a value, the function will do the same as <code>move</code></li>
                    <li>If the object is a reference and <code>T</code> is a value, the function will do the same as <code>deref</code></li>
                    <li>If <code>obj</code> is a reference and <code>T</code> is a mutable reference, the function will do the same as <code>demut</code></li>
                </ul>
                This function is useful when creating generic code for getting around references.
            </td>
        </tr>
    </tbody>
</table>

### cfwd

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>cfwd&lt;T>(obj: *) -> 'T</code> </td>
            <td rowspan="1">
                The same as <code>fwd</code>, but clones objects instead of moving them
            </td>
        </tr>
    </tbody>
</table>

### write_ptr_int

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>write_ptr_int(ptr: Pointer, offset: Int, value: Int)</code> </td>
            <td rowspan="1" style="width: 40%;">
                Writes an int (64 bits) to <code>ptr</code> with offset <code>value</code> (1 is 64 bits). Take into account that this function is <b>unsafe</b>
                and assumes that you allocated the memory using a native function
            </td>
        </tr>
    </tbody>
</table>

### write_ptr_float

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="1"> <code>write_ptr_float(ptr: Pointer, offset: Int, value: Float)</code> </td>
            <td rowspan="1" style="width: 40%;">
                Writes a double (64 bits) to <code>ptr</code> with offset <code>value</code> (1 is 64 bits). Take into account that this function is <b>unsafe</b>
                and assumes that you allocated the memory using a native function
            </td>
        </tr>
    </tbody>
</table>