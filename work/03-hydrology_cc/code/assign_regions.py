#!/usr/bin/env python3
# Assign CAMELS-CH gauges to the 7 ESSD bio-geographic regions (approximation of
# the FOEN scheme used in essd-15-5755-2023, Fig.4) from lon/lat/elevation.
import csv, io
TOPO = "data/camels_ch/static_attributes/CAMELS_CH_topographic_attributes.csv"
OUT  = "data/gauge_regions.csv"

def read_topo(path):
    rows=[]
    with open(path, encoding="latin-1") as f:
        for line in f:
            if line.startswith("#"): continue
            rows.append(line)
    return list(csv.DictReader(io.StringIO("".join(rows))))

def classify(lon, lat, elev, country):
    if lat >= 47.58 and 7.55 <= lon <= 8.7 and (country in ("DE",) or (elev==elev and elev < 1300)):
        return "Black Forest"
    if elev==elev and elev < 700 and 46.45 <= lat <= 47.6 and 6.3 <= lon <= 9.5:
        return "Plateau"
    if elev==elev and elev < 1600 and lat >= 47.0 and lon <= 7.9:
        return "Jura"
    if elev==elev and elev < 1500 and lat >= 47.3 and lon <= 8.3:
        return "Jura"
    if country == "I":
        return "Alps South"
    if lat < 46.30 and 7.6 <= lon <= 9.4:
        return "Alps South"
    if lat < 46.15:
        return "Alps South"
    if lon < 7.7:
        return "Alps West"
    if lon > 9.3:
        return "Alps East"
    return "Alps North"

def main():
    data=read_topo(TOPO); out=[]
    for d in data:
        try:
            lon=float(d["gauge_lon"]); lat=float(d["gauge_lat"])
            elev=float(d["elev_mean"]) if d.get("elev_mean") else float("nan")
        except: continue
        out.append({"gauge_id":d["gauge_id"],"gauge_name":d.get("gauge_name",""),
                    "country":d.get("country",""),"gauge_lon":lon,"gauge_lat":lat,
                    "elev_mean":elev,"region":classify(lon,lat,elev,d.get("country",""))})
    with open(OUT,"w",newline="") as f:
        w=csv.DictWriter(f,fieldnames=["gauge_id","gauge_name","country","gauge_lon","gauge_lat","elev_mean","region"])
        w.writeheader(); w.writerows(out)
    from collections import Counter
    c=Counter(r["region"] for r in out)
    print(f"Assigned {len(out)} gauges -> {OUT}")
    for k in ["Jura","Black Forest","Plateau","Alps North","Alps West","Alps South","Alps East"]:
        print(f"  {k:12}: {c.get(k,0)}")

if __name__=="__main__": main()
