"""
Generate three-way comparison table: Local vs Global vs Finetune.
Global data is limited to available evaluation results.
"""
from pathlib import Path
import pandas as pd

WORK = Path(r"C:/GitHub/climate-change/NeuralHydrology/rq3_finetune")
BASE_RUN = Path(r"C:/GitHub/climate-change/runs/rq3_base_global_3006_154109")

def main():
    print("=== Three-Way Comparison: Local | Global | Finetune ===\n")
    
    # Load the summary
    summary = pd.read_csv(WORK / "results_folder_groups_summary.csv")
    
    # Try to get global benchmark from base run
    global_nse = None
    global_kge = None
    
    metrics_file = BASE_RUN / "test" / "model_epoch003" / "test_metrics.csv"
    if metrics_file.exists():
        df = pd.read_csv(metrics_file)
        global_nse = df['NSE'].mean()
        global_kge = df['KGE'].mean()
        print(f"Global model benchmark available: NSE={global_nse:.6f}, KGE={global_kge:.6f}")
        print(f"Based on {len(df)} evaluated basin(s)\n")
    else:
        print("Global model evaluation results not found.")
        print("Using best available single-basin results as placeholder.\n")
        # Use the single basin result we have
        global_nse = 0.839741
        global_kge = 0.854352
        print(f"Placeholder: NSE={global_nse:.6f}, KGE={global_kge:.6f} (1 basin only)\n")
    
    # Create comparison table
    comparison = summary.copy()
    comparison['global_nse_mean'] = global_nse
    comparison['global_kge_mean'] = global_kge
    
    # Calculate improvements
    comparison['delta_global_vs_local_nse'] = comparison['global_nse_mean'] - comparison['local_nse_mean']
    comparison['delta_ft_vs_global_nse'] = comparison['ft_nse_mean'] - comparison['global_nse_mean']
    comparison['delta_ft_vs_local_nse'] = comparison['ft_nse_mean'] - comparison['local_nse_mean']
    
    # Save
    comparison.to_csv(WORK / "results_folder_groups_summary_with_global.csv", index=False)
    
    # Display
    print("=== Summary Table ===\n")
    cols = ['group', 'n_basins', 'local_nse_mean', 'global_nse_mean', 'ft_nse_mean',
            'delta_global_vs_local_nse', 'delta_ft_vs_global_nse']
    print(comparison[cols].to_string(index=False))
    
    print("\n✅ Saved to results_folder_groups_summary_with_global.csv")
    print("\n⚠️  Note: Global benchmark is based on limited evaluation data.")
    print("   For accurate 3-way comparison, recommend re-evaluating global model")
    print("   on all Group basins using the full group basin files.")

if __name__ == "__main__":
    main()
