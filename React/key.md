Keys serve as a hint to React but they don’t get passed to your components. If you need the same
value in your component, pass it explicitly as a prop with a different name:

```js
const content = posts.map((post) => <Post key={post.id} id={post.id} title={post.title} />);
```

With the example above, the Post component can read props.id, but not props.key.

# 关注子组件（children）的情况

https://segmentfault.com/a/1190000015366521#articleHeader7

当 diffing 的时候，如果 React 在检查 props.children 下的数组时，按顺序去对比数组内元素的话：index 0
将与 index 0 进行比较，index 1 和 index 1，等等。对于每一次对比，React 会使用之前提过的 diff 规则。
在我们的例子里，它认为 div 成为一个 span，那么就会运用到情景 3。这样不是很有效率的：想象一下，我们已
经从 1000 行中删除了第一行。React 将不得不“更新”剩余的 999 个子项，因为按 index 去对比的话，内容从第
一条开始就不相同了。

幸运的是，React 有一个内置的方法（built-in）来解决这个问题。如果一个元素有一个 key 属性，那么元素将
按 key 而不是 index 来比较。只要 key 是唯一的，React 就会移动元素，而不是将它们从 DOM 树中移除然后再
将它们放回（这个过程在 React 里叫 mounting 和 unmounting）。

```js
// ...
props: {
  children: [ // Now React will look on key, not index
    { type: 'div', key: 'div' },
    { type: 'span', key: 'span' },
    { type: 'br', key: 'bt' }
  ]
},
// ...
```
