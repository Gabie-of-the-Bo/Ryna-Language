<link rel="stylesheet" href="../../../../css/reference.css">

## Prefix

### - <span class="precedence">[ Precedence 300 ]</span>

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
                <code>- (n: Int) -> Int</code> 
            </td>
            <td rowspan="1">
                Negates <code>n</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>- (n: Float) -> Float</code> 
            </td>
        </tr>
</table>

## Binary

### + <span class="precedence">[ Precedence 650 ]</span>

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
                <code>(a: Int) + (b: Int) -> Int</code> 
            </td>
            <td rowspan="4">
                Calculates the sum of <code>a</code> and <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) + (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) + (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) + (b: Int) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: String) + (b: String) -> String</code> 
            </td>
        </tr>
    </tbody>
</table>

### - <span class="precedence">[ Precedence 700 ]</span>

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
                <code>(a: Int) - (b: Int) -> Int</code> 
            </td>
            <td rowspan="4">
                Calculates the difference between <code>a</code> and <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) - (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) - (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) - (b: Int) -> Float</code> 
            </td>
        </tr>
    </tbody>
</table>

### * <span class="precedence">[ Precedence 500 ]</span>

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
                <code>(a: Int) * (b: Int) -> Int</code> 
            </td>
            <td rowspan="4">
                Calculates the product of <code>a</code> and <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) * (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) * (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) * (b: Int) -> Float</code> 
            </td>
        </tr>
    </tbody>
</table>

### / <span class="precedence">[ Precedence 550 ]</span>

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
                <code>(a: Int) / (b: Int) -> Int</code> 
            </td>
            <td rowspan="4">
                Calculates the quotient between <code>a</code> and <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) / (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) / (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) / (b: Int) -> Float</code> 
            </td>
        </tr>
    </tbody>
</table>

### % <span class="precedence">[ Precedence 600 ]</span>

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
                <code>(a: Int) % (b: Int) -> Int</code> 
            </td>
            <td rowspan="4">
                Calculates the remainder of the division between <code>a</code> and <code>b</code>
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) % (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Int) % (b: Float) -> Float</code> 
            </td>
        </tr>
        <tr>
            <td> 
                <blockquote>
                    <p>All reference combinations are defined</p>
                </blockquote>
                <code>(a: Float) % (b: Int) -> Float</code> 
            </td>
        </tr>
    </tbody>
</table>