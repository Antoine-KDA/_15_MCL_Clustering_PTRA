# PTRA Experiment Management Scripts

## Quick Start

```bash
# 1. Make all scripts executable
chmod +x rj_run_job_nohup rj_check_status rj_quick_check rj_watch_experiment rj_stop_experiment

# 2. Launch experiment (runs for 2-5 days)
./rj_run_job_nohup

# 3. Check status anytime
./rj_quick_check          # Fast check
./rj_check_status         # Detailed check

# 4. Watch live (optional)
./rj_watch_experiment

# 5. Stop if needed
./rj_stop_experiment
```

## Files Overview

| Script | Purpose |
|--------|---------|
| `rj_run_job_nohup` | Launch experiment as background job |
| `rj_check_status` | Comprehensive status report |
| `rj_quick_check` | Fast daily status check |
| `rj_watch_experiment` | Real-time monitoring dashboard |
| `rj_stop_experiment` | Safely stop running experiment |
| `EXPERIMENT_GUIDE.txt` | Full documentation |

## Key Features

✅ **Survives SSH disconnects** - Experiment keeps running  
✅ **Automatic monitoring** - Logs resource usage every 5 minutes  
✅ **Safe to disconnect** - Close terminal/computer anytime  
✅ **Easy status checking** - Simple commands to monitor progress  
✅ **Graceful shutdown** - Stop experiment without data loss  

## Important Locations

```
~/projects/experiments/          # Script and log directory
  ├── experiment_YYYYMMDD_HHMMSS.log   # Main experiment log
  ├── experiment.pid                    # Process ID
  ├── experiment_status.txt             # Metadata
  └── experiment_monitor.log            # Resource usage log

~/projects/experiment1/          # Output directory
  ├── experiment_1_full_pop/            # Analysis results
  └── *.tar.gz                          # Compressed archives
```

## Daily Monitoring Routine

```bash
# Morning check
ssh into-your-server
cd ~/projects/experiments
./rj_quick_check

# Quick verification
ps aux | grep experiment_generic
tail -20 experiment_*.log

# Exit safely
exit
```

## Troubleshooting

**Experiment not starting?**
```bash
# Check if script exists
ls -l ~/projects/experiments/experiment_generic_full_population_analysis

# Check logs
tail -50 ~/projects/experiments/experiment_*.log
```

**Need more disk space?**
```bash
df -h ~/projects/
du -sh ~/projects/experiment1/
```

**System resources?**
```bash
free -h              # Memory
uptime               # Load average
df -h                # Disk space
```

## After Completion

```bash
# 1. Verify completion
./rj_check_status

# 2. Archive results
cd ~/projects/experiment1
tar -czf ../experiment_results_$(date +%Y%m%d).tar.gz .

# 3. Download results (from local machine)
scp user@server:~/projects/experiment_results_*.tar.gz .
```

## Support

For detailed information, see: `EXPERIMENT_GUIDE.txt`

Created by: Antoine KDA  
Date: November 2025
