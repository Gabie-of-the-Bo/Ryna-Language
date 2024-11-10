### load_library

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>load_library(path: String) -> Library</code> </td>
            <td rowspan="6">
                Read the dynamic library at <code>path</code>
            </td>
        </tr>
    </tbody>
</table>

### get_function

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>get_function(lib: &Library, function_name: String) -> LibraryFunction</code> </td>
            <td rowspan="6" style="width: 25%;">
                Gets the <code>function_name</code> function from <code>lib</code> 
            </td>
        </tr>
    </tbody>
</table>

### call

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td> <code>call(func: &LibraryFunction, arg1: *, ...) -> *</code> </td>
            <td style="width: 45%;">
                Calls the native function <code>func</code> with the given arguments
            </td>
        </tr>
    </tbody>
</table>