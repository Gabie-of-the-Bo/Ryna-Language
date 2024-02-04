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
                <code>! (v: Bool) -> Bool</code> 
            </td>
            <td rowspan="3">
                Negates <code>v</code>
            </td>
        </tr>
        <tr>
            <td>
                <code>! (v: &Bool) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td>
                <code>! (v: @Bool) -> Bool</code> 
            </td>
        </tr>
</table>

## Binary

### && <span class="precedence">[ Precedence 1500 ]</span>

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
                <code>(a: Bool) && (b: Bool) -> Bool</code> 
            </td>
            <td rowspan="1">
                Calculates the logical operation <code>a</code> ∧ <code>b</code>. It short-circuits
            </td>
        </tr>
    </tbody>
</table>

### || <span class="precedence">[ Precedence 1550 ]</span>

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
                <code>(a: Bool) || (b: Bool) -> Bool</code> 
            </td>
            <td rowspan="1">
                Calculates the logical operation <code>a</code> ∨ <code>b</code>. It short-circuits
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
                <code>(a: Bool) ^ (b: Bool) -> Bool</code> 
            </td>
            <td rowspan="1">
                Calculates the logical operation <code>a</code> XOR <code>b</code>
            </td>
        </tr>
    </tbody>
</table>

### < <span class="precedence">[ Precedence 900 ]</span>

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
                <code>(a: Int) < (b: Int) -> Bool</code> 
            </td>
            <td rowspan="4">
                Calculates whether or not <code>a</code> is lower than <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) < (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) < (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) < (b: Int) -> Bool</code> 
            </td>
        </tr>
    </tbody>
</table>

### <= <span class="precedence">[ Precedence 1000 ]</span>

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
                <code>(a: Int) <= (b: Int) -> Bool</code> 
            </td>
            <td rowspan="4">
                Calculates whether or not <code>a</code> is lower or equal to <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) <= (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) <= (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) <= (b: Int) -> Bool</code> 
            </td>
        </tr>
    </tbody>
</table>

### > <span class="precedence">[ Precedence 950 ]</span>

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
                <code>(a: Int) > (b: Int) -> Bool</code> 
            </td>
            <td rowspan="4">
                Calculates whether or not <code>a</code> is greater than <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) > (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) > (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) > (b: Int) -> Bool</code> 
            </td>
        </tr>
    </tbody>
</table>

### >= <span class="precedence">[ Precedence 1050 ]</span>

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
                <code>(a: Int) >= (b: Int) -> Bool</code> 
            </td>
            <td rowspan="4">
                Calculates whether or not <code>a</code> is greater or equal to <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) >= (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) >= (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) >= (b: Int) -> Bool</code> 
            </td>
        </tr>
    </tbody>
</table>

### == <span class="precedence">[ Precedence 1100 ]</span>

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
                <code>(a: Int) == (b: Int) -> Bool</code> 
            </td>
            <td rowspan="4">
                Calculates whether or not <code>a</code> is equal to <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) == (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Bool) == (b: Bool) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: String) == (b: String) -> Bool</code> 
            </td>
        </tr>
    </tbody>
</table>

### != <span class="precedence">[ Precedence 1150 ]</span>

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
                <code>(a: Int) != (b: Int) -> Bool</code> 
            </td>
            <td rowspan="4">
                Calculates whether or not <code>a</code> is different to <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) != (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) != (b: Float) -> Bool</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) != (b: Int) -> Bool</code> 
            </td>
        </tr>
    </tbody>
</table>