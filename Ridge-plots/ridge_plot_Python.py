#-------------------------------------------------
# Ridgeplots:
# Different distribution density on the same graph
# on different levels
#-------------------------------------------------

import seaborn as sns
import matplotlib.pyplot as plt

# load iris data
iris = sns.load_dataset('iris')

# ploting
sns.set_theme(style="white", rc={"axes.facecolor": (0, 0, 0, 0), 'axes.linewidth':1})
palette = sns.color_palette("Set1", 3)
g = sns.FacetGrid(iris, row="species", palette = palette, hue="species", aspect=5, height=1.5)
g.map_dataframe(sns.kdeplot, x="sepal_length", fill= True, alpha=0.4)
def label(x, color, label):
    ax = plt.gca()
    ax.text(0, .2, label, color='black', fontsize=13,
            ha="left", va="center", transform=ax.transAxes)
g.map(label, "species")
g.fig.subplots_adjust(hspace=-.5)
g.set_titles("")
g.set(yticks=[], ylabel ="", xlabel="Sepal Length")
plt.suptitle('Ridge plot with Seaborn on iris dataset', y=0.98)

#----
# end
#----