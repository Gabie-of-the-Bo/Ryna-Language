<link rel="stylesheet" href="../../../../css/reference.css">

## Prefix

### ! <span class="precedence">[ Precedence 250 ]</span>

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
                <code>! (n: Int) -> Int</code> 
            </td>
            <td rowspan="3">
                Negates the bits of <code>n</code>
            </td>
        </tr>
        <tr>
            <td>
                <code>! (n &Int) -> Int</code> 
            </td>
        </tr>
        <tr>
            <td>
                <code>! (n @Int) -> Int</code> 
            </td>
        </tr>
</table>

## Binary

### & <span class="precedence">[ Precedence 370 ]</span>

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
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) & (b: Int) -> Int</code> 
            </td>
            <td rowspan="1">
                Calculates the bitwise operation <code>a</code> ∧ <code>b</code>
            </td>
        </tr>
    </tbody>
</table>

### | <span class="precedence">[ Precedence 380 ]</span>

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
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) | (b: Int) -> Int</code> 
            </td>
            <td rowspan="1">
                Calculates the bitwise operation <code>a</code> ∨ <code>b</code>
            </td>
        </tr>
    </tbody>
</table>

### >> <span class="precedence">[ Precedence 350 ]</span>

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
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) >> (b: Int) -> Int</code> 
            </td>
            <td rowspan="1">
                Calculates <code>a</code> with <code>b</code> bits shifted to the right
            </td>
        </tr>
    </tbody>
</table>

### << <span class="precedence">[ Precedence 360 ]</span>

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
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) << (b: Int) -> Int</code> 
            </td>
            <td rowspan="1">
                Calculates <code>a</code> with <code>b</code> bits shifted to the left
            </td>
        </tr>
    </tbody>
</table>

### ^ <span class="precedence">[ Precedence 390 ]</span>

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
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) ^ (b: Int) -> Int</code> 
            </td>
            <td rowspan="1">
                Calculates the bitwise operation <code>a</code> XOR <code>b</code>
            </td>
        </tr>
    </tbody>
</table>