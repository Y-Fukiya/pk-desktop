#!/usr/bin/env python3
"""Independent arithmetic reference for this one synthetic NCA example.

Uses Python's standard library only; it does not import or call PK Desktop,
PKNCA, R, or admiral. Terminal points are specified as 2--8 h after Cmax.
"""
from __future__ import annotations
import argparse, csv, math
from pathlib import Path

HERE = Path(__file__).resolve().parent
CODES = ('CMAX','TMAX','AUCLST','LAMZ','LAMZHL','AUCIFO','AUCPEO','CMAXD','AUCIFOD','CLFO','VZFO')
UNITS = ('mg/L','h','h*mg/L','1/h','h','h*mg/L','%','1/L','h/L','L/h','L')
FIELDS = ('STUDYID','USUBJID','PERIOD','TRT','ANALYTE','NCAPCD','PARAMCD','AVAL','AVALU','NCAPSTAT')

def rows():
    with (HERE/'concentrations.csv').open(newline='',encoding='utf-8') as f: data=list(csv.DictReader(f))
    groups={}
    for r in data: groups.setdefault(tuple(r[k] for k in ('STUDYID','USUBJID','PERIOD','TRT','ANALYTE')),[]).append(r)
    if len(groups)!=6: raise ValueError('Expected six synthetic profiles')
    output=[]
    for key,group in sorted(groups.items()):
        points=sorted((float(r['TIME']),float(r['CONC'])) for r in group)
        if len(set(t for t,_ in points))!=len(points) or any(r['UNIT']!='mg/L' or r['DOSEU']!='mg' or r['ROUTE']!='ORAL' or float(r['DOSE'])!=100 for r in group): raise ValueError('Unexpected input')
        maximum=max(c for _,c in points);tmax=next(t for t,c in points if c==maximum)
        area=0.0
        for (t0,c0),(t1,c1) in zip(points,points[1:]):
            dt=t1-t0
            area+=dt*(c0-c1)/math.log(c0/c1) if c0>c1>0 else dt*(c0+c1)/2
        terminal=[(t,math.log(c)) for t,c in points if 2<=t<=8 and t>tmax and c>0]
        lam=half=inf=ext=norminf=clear=volume=None
        if len(terminal)>=3:
            mean_t=sum(t for t,_ in terminal)/len(terminal);mean_y=sum(y for _,y in terminal)/len(terminal)
            slope=sum((t-mean_t)*(y-mean_y) for t,y in terminal)/sum((t-mean_t)**2 for t,_ in terminal)
            lam=-slope
            if lam<=0: raise ValueError('Invalid terminal slope')
            half=math.log(2)/lam; inf=area+points[-1][1]/lam;ext=100*(inf-area)/inf
            norminf=inf/100;clear=100/inf;volume=clear/lam
        values=(maximum,tmax,area,lam,half,inf,ext,maximum/100,norminf,clear,volume)
        for code,unit,value in zip(CODES,UNITS,values):
            output.append(dict(zip(FIELDS,(*key,code,code+('M' if key[-1]=='METAB' else ''),'' if value is None else format(value,'.17g'),unit,'not_calculable' if value is None else 'calculated'))))
    return output

def main():
    parser=argparse.ArgumentParser();g=parser.add_mutually_exclusive_group(required=True);g.add_argument('--write',type=Path);g.add_argument('--check',type=Path);a=parser.parse_args();expected=rows()
    if a.write:
        with a.write.open('x',newline='',encoding='utf-8') as f:
            writer=csv.DictWriter(f,fieldnames=FIELDS,lineterminator='\n');writer.writeheader();writer.writerows(expected)
    else:
        with a.check.open(newline='',encoding='utf-8') as f: actual=list(csv.DictReader(f))
        if len(actual)!=len(expected): raise SystemExit('Reference row count differs')
        for arow,erow in zip(actual,expected):
            for k in FIELDS:
                if k=='AVAL' and arow[k] and erow[k]:
                    if not math.isclose(float(arow[k]),float(erow[k]),rel_tol=1e-13,abs_tol=1e-13): raise SystemExit('Reference numeric mismatch')
                elif arow[k]!=erow[k]: raise SystemExit('Reference field mismatch')
    print('PASS independent Python reference:',len(expected),'rows;',sum(bool(r['AVAL']) for r in expected),'numeric values')
if __name__=='__main__': main()
