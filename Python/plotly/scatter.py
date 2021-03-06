import plotly
import plotly.graph_objs as go

trace1 = go.Scatter(
    x=[0, 1, 2, 3, 4, 5, 6, 7, 8],
    y=[0, 1, 2, 3, 4, 5, 6, 7, 8],
    mode="markers",
    name="Name of Trace 1",
)
trace2 = go.Scatter(
    x=[0, 1, 2, 3, 4, 5, 6, 7, 8],
    y=[1, 0, 3, 2, 5, 4, 7, 6, 8],
    mode="markers",
    name="Name of Trace 2",
)
data = [trace1, trace2]
layout = go.Layout(
    title="Plot Title",
    xaxis=dict(
        title="x Axis",
        titlefont=dict(family="Courier New, monospace", size=18, color="#7f7f7f"),
    ),
    yaxis=dict(
        title="y Axis",
        titlefont=dict(family="Courier New, monospace", size=18, color="#7f7f7f"),
    ),
)
fig = go.Figure(data=data, layout=layout)
plot_url = plotly.offline.plot(fig, filename="styling-names.html")
