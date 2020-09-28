# Convolve

A particularly useful intuitive explanation that works well for discrete signals is to think of
convolution as a "weighted sum of echoes" or "weighted sum of memories."

https://stackoverflow.com/questions/42883547/intuitive-understanding-of-1d-2d-and-3d-convolutions-in-convolutional-neural-n

## Convolve all colors using skimage

```python
def convolve_all_colours(im, window):
    """
    Convolves im with window, over all three colour channels
    """
    ims = []
    for d in range(3):
        im_conv_d = convolve2d(im[:, :, d], window, mode="same", boundary="symm")
        ims.append(im_conv_d)

    im_conv = np.stack(ims, axis=2).astype("uint8")

    return im_conv

```

## 1D: http://www.songho.ca/dsp/convolution/convolution.html#convolution_1d

The convolution of two vectors, `u` and `v`, represents the area of overlap under the points as `v`
slides across `u`. Algebraically, convolution is the same operation as multiplying polynomials whose
coefficients are the elements of `u` and `v`.

Let `m = length(u)` and `n = length(v)` . Then `w` is the vector of length `m+n-1` whose `k`th
element is

$y[n]=\sum_{k}x(k) \cdot h(n-k)$

The sum is over all the values of `j` that lead to legal subscripts for `u(j)` and `v(k-j+1)`,
specifically `j` `=` `max(1,k+1-n):1:min(k,m)`. When `m` `=` `n`, this gives

```
w(1) = u(1)*v(1)
w(2) = u(1)*v(2)+u(2)*v(1)
w(3) = u(1)*v(3)+u(2)*v(2)+u(3)*v(1)
...
w(n) = u(1)*v(n)+u(2)*v(n-1)+ ... +u(n)*v(1)
...
w(2*n-1) = u(n)*v(n)
```

## 2D: http://www.songho.ca/dsp/convolution/convolution.html#convolution_1d

$y[m,n] =
x[m,n]*h[m,n]=\sum_{j=-\infty}^{\infty}\sum_{i=-\infty}^{\infty}x[i,j]\cdot h[m-i,n-j]$

```c++
// find center position of kernel (half of kernel size)
kCenterCol = kCols / 2;
kCenterRow = kRows / 2;

for (i = 0; i < rows; ++i) // rows
{
  for (j = 0; j < cols; ++j) // columns
  {
    for (m = 0; m < kRows; ++m) // kernel rows
    {
      mm = kRows - 1 - m; // row index of flipped kernel

      for (n = 0; n < kCols; ++n) // kernel columns
      {
        nn = kCols - 1 - n; // column index of flipped kernel

        // index of input signal, used for checking boundary
        ii = i + (kCenterRow - mm);
        jj = j + (kCenterCol - nn);

        // ignore input samples which are out of bound
        if (ii >= 0 && ii < rows && jj >= 0 && jj < cols)
          out[i][j] += in[ii][jj] * kernel[mm][nn];
      }
    }
  }
}
```
