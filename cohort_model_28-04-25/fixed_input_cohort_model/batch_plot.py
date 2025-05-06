import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
files = [
    ('Initial_SOC.csv', 'Initial SOC'),
    ('absolute_SOC_2050.csv', 'Absolute SOC 2050'),
    ('absolute_SOC_2080.csv', 'Absolute SOC 2080'),
    ('delta_SOC_2050.csv', 'Delta SOC 2050'),
    ('delta_SOC_2080.csv', 'Delta SOC 2080'),
    ('tree_carbon_2050.csv', 'Tree Carbon 2050'),
    ('tree_carbon_2080.csv', 'Tree Carbon 2080'),
    ('cumulative_tree_carbon_2050.csv', 'Cumulative Tree Carbon 2050'),
    ('cumulative_tree_carbon_2080.csv', 'Cumulative Tree Carbon 2080'),
]

for fname, title in files:
    data = np.loadtxt(fname, delimiter=',')
    x = data[:,0]; y = data[:,1]; vals = data[:,2]
    sf = 1000.0
    grid_shape = (1+int((y.max()-y.min())/sf), 1+int((x.max()-x.min())/sf))
    grid = np.full(grid_shape, np.nan)
    yi = ((y-y.min())/sf).astype(int)
    xi = ((x-x.min())/sf).astype(int)
    np.clip(yi, 0, grid_shape[0]-1, out=yi)
    np.clip(xi, 0, grid_shape[1]-1, out=xi)
    grid[yi, xi] = vals
    fig, ax = plt.subplots()
    ax.set_axis_off()
    im = ax.imshow(np.ma.masked_invalid(grid), origin='lower', interpolation='nearest', cmap='gist_ncar')
    cbar = plt.colorbar(im, shrink=0.5)
    ax.set_title(title)
    cbar.set_label('SOC (t C ha$^{-1}$)', rotation=270, labelpad=15)
    ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: f'{int(x)}'))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda y, _: f'{int(y)}'))
    out = title.replace(' ','_') + '.png'
    fig.savefig(out, dpi=500, bbox_inches='tight')
    plt.close(fig)
