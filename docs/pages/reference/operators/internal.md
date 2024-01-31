<link rel="stylesheet" href="../../../../css/reference.css">

## Binary

### := <span class="precedence">[ Precedence 100000 ]</span>

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>
                <code>&lt;T> (a: @'T) := (b: 'T) -> ()</code> 
            </td>
            <td rowspan="1">
                Assigns <code>b</code> to the value to which <code>b</code> is pointing
            </td>
        </tr>
    </tbody>
</table>

## N-ary

### ( ) <span class="precedence">[ Precedence 50 ]</span>

<table>
    <thead>
        <tr>
            <th>Overload</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>
                <code>&lt;R> (f: () => 'R)() -> R</code> 
            </td>
            <td rowspan="3">
                Calls <code>f</code> with the provided arguments
            </td>
        </tr>
        <tr>
            <td>
                <code>&lt;A1, R> (f: 'A1 => 'R)(a1: 'A1) -> R</code> 
            </td>
        </tr>
        <tr>
            <td>
                <code>&lt;A1, A2, R> (f: ('A1, 'A2) => 'R)(a1: 'A1, a2: 'A2) -> R</code> 
            </td>
        </tr>
        <tr>
            <td>
                <blockquote>
                    <p>Defined until length 30</p>
                </blockquote>
            </td>
        </tr>
    </tbody>
</table>