"""Ward-clustered feature/resolution comparison for the two-image benchmark."""
import argparse
import csv
import json
from pathlib import Path
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from scipy.cluster.hierarchy import linkage, dendrogram


def feature_names():
    return ([f'{plane} {stat}' for plane in ['Red','Green','Blue','Gray','Chroma']
             for stat in ['mean','std','p05','p25','p50','p75','p95']]
            +[f'Lag {lag:02d} {direction} {stat}' for lag in [1,2,4,8,16]
              for direction in ['vertical','horizontal'] for stat in ['mean','std','p90']]
            +[f'Grid r{r+1} c{c+1} {stat}' for r in range(3) for c in range(4) for stat in ['mean','std']])


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--input',type=Path,required=True);p.add_argument('--output',type=Path,required=True)
    a=p.parse_args();a.output.mkdir(parents=True,exist_ok=True)
    data=json.loads(a.input.read_text());vectors=[];samples=[]
    for row in data['results']:
        for i,image in enumerate(row['images']):
            vectors.append(image['vector']);samples.append(f'{"AB"[i]} · {row["size"][0]}×{row["size"][1]}')
    raw=np.array(vectors,dtype=float);names=feature_names()
    if raw.shape!=(12,89) or not np.isfinite(raw).all():raise ValueError('Expected finite 12 × 89 benchmark')
    sd=raw.std(axis=0);z=np.divide(raw-raw.mean(axis=0),sd,out=np.zeros_like(raw),where=sd>1e-12)
    sample_tree=linkage(z,method='ward',optimal_ordering=True)
    feature_tree=linkage(z.T,method='ward',optimal_ordering=True)
    fig=plt.figure(figsize=(16,23))
    grid=fig.add_gridspec(2,3,width_ratios=[2.3,9,.18],height_ratios=[2,18],
        left=.025,right=.78,bottom=.12,top=.92,wspace=.025,hspace=.025)
    top=fig.add_subplot(grid[0,1]);left=fig.add_subplot(grid[1,0]);heat=fig.add_subplot(grid[1,1]);strip=fig.add_subplot(grid[1,2])
    col=dendrogram(sample_tree,ax=top,no_labels=True,color_threshold=0,above_threshold_color='#445566')['leaves']
    row=dendrogram(feature_tree,ax=left,orientation='left',no_labels=True,color_threshold=0,above_threshold_color='#445566')['leaves']
    top.set_axis_off();left.set_axis_off()
    values=z.T[np.ix_(row,col)]
    vmax=max(abs(values.min()),abs(values.max()))
    image=heat.imshow(values,origin='lower',aspect='auto',extent=[0,120,0,890],
        cmap='RdBu_r',vmin=-vmax,vmax=vmax,interpolation='nearest')
    heat.set_xticks(np.arange(12)*10+5,[samples[i] for i in col],rotation=55,ha='right',fontsize=9)
    heat.set_yticks(np.arange(89)*10+5,[names[i] for i in row],fontsize=7.5)
    heat.yaxis.tick_right();heat.tick_params(axis='y',pad=35,length=0)
    groups=np.array([0]*35+[1]*30+[2]*24)
    strip.imshow(groups[row,None],origin='lower',aspect='auto',extent=[0,1,0,890],
        cmap=ListedColormap(['#e5a43b','#578fc0','#66aa77']),vmin=0,vmax=2,interpolation='nearest');strip.set_axis_off()
    colorbar=fig.colorbar(image,cax=fig.add_axes([.83,.87,.015,.07]));colorbar.set_label('Feature z-score',fontsize=10)
    fig.suptitle('89 features × 2 images × 6 resolutions\nWard clustering of samples and features',fontsize=19,y=.975)
    fig.text(.025,.937,'A: Sep 21 · rippled surface   |   B: Sep 15 · sheet boundaries / rubble',fontsize=11)
    fig.text(.025,.023,'Each feature standardized across these 12 samples; red = relatively high, blue = relatively low.\n'
        'Ward uses Euclidean distances, with optimal leaf ordering. Pixel lags stay fixed as resolution changes.\n'
        'Feature strip: gold = colour/brightness (35), blue = texture (30), green = spatial grid (24).\n'
        'Exploratory comparison of two photos, not evidence of classifier accuracy.',fontsize=10)
    for extension in ('png','svg','pdf'):fig.savefig(a.output/f'ward-heatmap.{extension}',dpi=150)
    for filename,values in [('raw-features.csv',raw),('standardized-features.csv',z)]:
        with (a.output/filename).open('w',newline='') as f:
            w=csv.writer(f);w.writerow(['sample',*names]);w.writerows([label,*v] for label,v in zip(samples,values))
    (a.output/'clustering.json').write_text(json.dumps(dict(sample_order=[samples[i] for i in col],feature_order=[names[i] for i in row],sample_linkage=sample_tree.tolist(),feature_linkage=feature_tree.tolist()),indent=2))
    print(a.output/'ward-heatmap.png')


if __name__=='__main__':main()
