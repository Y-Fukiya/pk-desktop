#!/usr/bin/env python3
"""Standalone Python reference for trailing-point automatic lambda selection.

The only intended scope is the synthetic example. It imports no R, PKNCA,
PK Desktop or admiral code. Candidate windows end at the last observation.
"""
from __future__ import annotations
import argparse, csv, math
from pathlib import Path
HERE=Path(__file__).resolve().parent
CODES=('CMAX','TMAX','AUCLST','LAMZ','LAMZHL','AUCIFO','AUCPEO','CMAXD','AUCIFOD','CLFO','VZFO')
UNITS=('mg/L','h','h*mg/L','1/h','h','h*mg/L','%','1/L','h/L','L/h','L')
FIELDS=('STUDYID','USUBJID','PERIOD','TRT','ANALYTE','NCAPCD','PARAMCD','AVAL','AVALU','NCAPSTAT','TERM_START','TERM_END','TERM_N')

def fit(points):
    n=len(points);xs=[t for t,_ in points];ys=[math.log(c) for _,c in points]
    xm=sum(xs)/n;ym=sum(ys)/n;xx=sum((x-xm)**2 for x in xs);yy=sum((y-ym)**2 for y in ys)
    if xx<=0 or yy<=0: return None
    slope=sum((x-xm)*(y-ym) for x,y in zip(xs,ys))/xx
    if slope>=0: return None
    intercept=ym-slope*xm
    sse=sum((y-(intercept+slope*x))**2 for x,y in zip(xs,ys))
    r2=1-sse/yy;adjusted=1-(1-r2)*(n-1)/(n-2)
    return {'points':points,'lam':-slope,'adjusted':adjusted}

def rows():
    with (HERE/'concentrations.csv').open(newline='',encoding='utf-8') as f:data=list(csv.DictReader(f))
    groups={}
    for r in data:groups.setdefault(tuple(r[k] for k in ('STUDYID','USUBJID','PERIOD','TRT','ANALYTE')),[]).append(r)
    if len(groups)!=6:raise ValueError('Expected six synthetic profiles')
    output=[]
    for key,group in sorted(groups.items()):
        points=sorted((float(r['TIME']),float(r['CONC'])) for r in group)
        if len(set(t for t,_ in points))!=len(points) or any(r['UNIT']!='mg/L' or r['DOSEU']!='mg' or r['ROUTE']!='ORAL' or float(r['DOSE'])!=100 for r in group):raise ValueError('Unexpected input')
        maximum=max(c for _,c in points);tmax=next(t for t,c in points if c==maximum)
        area=0.0
        for (t0,c0),(t1,c1) in zip(points,points[1:]):
            dt=t1-t0;area+=dt*(c0-c1)/math.log(c0/c1) if c0>c1>0 else dt*(c0+c1)/2
        eligible=[(t,c) for t,c in points if t>tmax and t>0 and c>0]
        fits=[x for n in range(3,len(eligible)+1) if (x:=fit(eligible[-n:])) is not None]
        selected=None
        if fits:
            best=max(x['adjusted'] for x in fits)
            selected=max((x for x in fits if x['adjusted']>best-0.0001),key=lambda x:len(x['points']))
        lam=half=inf=ext=norminf=clear=volume=None
        start=end=term_n=''
        if selected is not None:
            lam=selected['lam'];half=math.log(2)/lam;inf=area+points[-1][1]/lam;ext=100*(inf-area)/inf
            norminf=inf/100;clear=100/inf;volume=clear/lam
            start=format(selected['points'][0][0],'.17g');end=format(selected['points'][-1][0],'.17g');term_n=str(len(selected['points']))
        values=(maximum,tmax,area,lam,half,inf,ext,maximum/100,norminf,clear,volume)
        for code,unit,value in zip(CODES,UNITS,values):
            output.append(dict(zip(FIELDS,(*key,code,code+('M' if key[-1]=='METAB' else ''),'' if value is None else format(value,'.17g'),unit,'not_calculable' if value is None else 'calculated',start,end,term_n))))
    return output

def main():
    parser=argparse.ArgumentParser();g=parser.add_mutually_exclusive_group(required=True);g.add_argument('--write',type=Path);g.add_argument('--check',type=Path);a=parser.parse_args();expected=rows()
    if a.write:
        with a.write.open('x',newline='',encoding='utf-8') as f:w=csv.DictWriter(f,fieldnames=FIELDS,lineterminator='\n');w.writeheader();w.writerows(expected)
    else:
        with a.check.open(newline='',encoding='utf-8') as f:actual=list(csv.DictReader(f))
        if len(actual)!=len(expected):raise SystemExit('Reference row count differs')
        for actual_row,expected_row in zip(actual,expected):
            for k in FIELDS:
                if k=='AVAL' and actual_row[k] and expected_row[k]:
                    if not math.isclose(float(actual_row[k]),float(expected_row[k]),rel_tol=1e-13,abs_tol=1e-13):raise SystemExit('Reference numeric mismatch')
                elif actual_row[k]!=expected_row[k]:raise SystemExit('Reference field mismatch')
    print('PASS independent auto-terminal Python reference:',len(expected),'rows;',sum(bool(r['AVAL']) for r in expected),'numeric values')
if __name__=='__main__':main()
