#!/usr/bin/env python3
# =============================================================================
# Assign CAMELS-CH catchments to the official FOEN bio-geographic regions via a
# proper SPATIAL OVERLAY (not a point rule). This directly addresses the
# "a river crosses several regions" problem:
#   - read official FOEN regions (N2020_Revision_BiogeoRegion.shp, 6 main regions,
#     EPSG:2056) and CAMELS-CH catchment polygons (same CRS);
#   - for each catchment, intersect with every region and measure the area share;
#   - assign the region with the LARGEST area share as the dominant region;
#   - record the cross-region fraction (1 - dominant share) and the secondary
#     region, so trans-regional catchments are flagged explicitly.
# The 7th region (southern Black Forest, used in essd-15-5755-2023) is not in the
# Swiss FOEN layer, so German catchments whose centroid falls outside all FOEN
# polygons are labelled "Black Forest" (consistent with the paper).
# =============================================================================
import csv, os
import shapefile
from shapely.geometry import shape, Point
from shapely.ops import unary_union
from shapely.validation import make_valid

REGION_SHP = "data/biogeographische_regionen_2056.shp/N2020_Revision_BiogeoRegion.shp"
CATCH_SHP  = "data/camels_ch/catchment_delineations/CAMELS_CH_catchments.shp"
TOPO       = "data/camels_ch/static_attributes/CAMELS_CH_topographic_attributes.csv"
OUT        = "data/gauge_regions_official.csv"

# FOEN RegionNumm -> our English label (matching the paper's naming)
FOEN_LABEL = {1:"Jura", 2:"Plateau", 3:"Alps North",
              4:"Alps West", 5:"Alps East", 6:"Alps South"}

def load_regions():
    """Return list of (label, shapely_geom) merging sub-regions by main RegionNumm."""
    sf=shapefile.Reader(REGION_SHP)
    flds=[f[0] for f in sf.fields[1:]]
    ri=flds.index("RegionNumm")
    geoms={}
    for shp,rec in zip(sf.shapes(), sf.records()):
        num=int(rec[ri]); g=make_valid(shape(shp.__geo_interface__))
        geoms.setdefault(num, []).append(g)
    merged={}
    for num,gl in geoms.items():
        merged[FOEN_LABEL[num]] = unary_union(gl)
    return merged

def load_country():
    """gauge_id -> country, from topographic attrs (latin-1)."""
    out={}
    with open(TOPO, encoding="latin-1") as f:
        rows=[l for l in f if not l.startswith("#")]
    import io
    for d in csv.DictReader(io.StringIO("".join(rows))):
        out[str(int(float(d["gauge_id"])))]=d.get("country","")
    return out

def main():
    regions=load_regions()
    country=load_country()
    sf=shapefile.Reader(CATCH_SHP)
    flds=[f[0] for f in sf.fields[1:]]
    gi=flds.index("gauge_id")

    out_rows=[]
    n_cross=0
    for shp,rec in zip(sf.shapes(), sf.records()):
        gid=str(int(float(rec[gi])))
        catch=make_valid(shape(shp.__geo_interface__))
        total=catch.area if catch.area>0 else 1.0

        shares={}
        for label,rgeom in regions.items():
            try:
                inter=catch.intersection(rgeom).area
            except Exception:
                inter=0.0
            if inter>0:
                shares[label]=inter/total

        covered_tot=sum(shares.values()) if shares else 0.0
        # Use area-overlay only when the catchment is meaningfully covered by the
        # FOEN layer (>=20%). Below that the catchment is mostly cross-border, and
        # "largest intersection" is unreliable -> fall back to nearest-neighbour.
        if shares and covered_tot>=0.20:
            ordered=sorted(shares.items(), key=lambda kv: kv[1], reverse=True)
            dom_region, dom_share = ordered[0]
            sec_region = ordered[1][0] if len(ordered)>1 else ""
            sec_share  = ordered[1][1] if len(ordered)>1 else 0.0
            covered=covered_tot
            assign_method="overlay"
        else:
            # outside the Swiss FOEN coverage (cross-border AT/FR/IT or DE).
            ctry=country.get(gid,"")
            if ctry=="DE":
                # southern Black Forest = the paper's 7th region
                dom_region="Black Forest"
            else:
                # nearest-neighbour: assign to the FOEN region whose boundary is
                # closest to the catchment centroid (extends regions to neighbours,
                # as essd-15-5755-2023 does).
                cen=catch.centroid
                dom_region=min(regions.items(), key=lambda kv: cen.distance(kv[1]))[0]
            dom_share=0.0; sec_region=""; sec_share=0.0; covered=0.0
            assign_method="nearest" if ctry!="DE" else "country"

        # German catchments mostly outside CH boundary -> Black Forest (7th region)
        if country.get(gid)=="DE" and (dom_share < 0.5):
            dom_region="Black Forest"; assign_method="country"

        cross = (assign_method=="overlay") and dom_share < 0.80
        if cross: n_cross+=1

        out_rows.append({
            "gauge_id":gid, "country":country.get(gid,""),
            "region":dom_region,
            "dominant_share":round(dom_share,3),
            "second_region":sec_region,
            "second_share":round(sec_share,3),
            "foen_coverage":round(covered,3),
            "cross_region": "yes" if cross else "no",
            "assign_method":assign_method,
        })

    with open(OUT,"w",newline="") as f:
        w=csv.DictWriter(f, fieldnames=["gauge_id","country","region","dominant_share",
                                        "second_region","second_share","foen_coverage",
                                        "cross_region","assign_method"])
        w.writeheader(); w.writerows(out_rows)

    from collections import Counter
    c=Counter(r["region"] for r in out_rows)
    print(f"分区完成 {len(out_rows)} 个流域 -> {OUT}")
    for k in ["Jura","Black Forest","Plateau","Alps North","Alps West","Alps South","Alps East","NA"]:
        if c.get(k,0): print(f"  {k:12}: {c.get(k,0)}")
    print(f"\n跨区流域(主区面积占比<80%): {n_cross} ({100*n_cross/len(out_rows):.0f}%)")
    # 跨区程度分布
    crosses=[r for r in out_rows if r["cross_region"]=="yes"]
    if crosses:
        print("跨区流域示例(主区占比最低的几个):")
        for r in sorted(crosses, key=lambda r:r["dominant_share"])[:8]:
            print(f"  {r['gauge_id']}: {r['region']} {r['dominant_share']*100:.0f}% + "
                  f"{r['second_region']} {r['second_share']*100:.0f}%")

if __name__=="__main__":
    main()
