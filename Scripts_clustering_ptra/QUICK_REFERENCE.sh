#!/bin/bash
# QUICK REFERENCE - PTRA Experiment Management
# ==============================================

# LAUNCH EXPERIMENT
# -----------------
./rj_run_job_nohup


# CHECK STATUS
# ------------
./rj_quick_check          # Fast check (5 seconds)
./rj_check_status         # Detailed report
./rj_watch_experiment     # Live monitoring (Ctrl+C to exit)


# STOP EXPERIMENT
# ---------------
./rj_stop_experiment      # Safe shutdown


# MANUAL CHECKS
# -------------
ps aux | grep experiment_generic                   # Is it running?
tail -f ~/projects/experiments/experiment_*.log    # View log live
tail -20 ~/projects/experiments/experiment_*.log   # Last 20 lines


# FILES LOCATION
# --------------
~/projects/experiments/experiment_*.log     # Main log
~/projects/experiments/experiment.pid       # Process ID
~/projects/experiments/experiment_monitor.log    # Resource usage
~/projects/experiment1/                     # Output files


# SYSTEM CHECKS
# -------------
free -h                   # Memory
df -h ~/projects/         # Disk space
uptime                    # System load


# FIRST TIME SETUP
# ----------------
chmod +x rj_*             # Make all scripts executable
./rj_run_job_nohup        # Launch experiment
./rj_quick_check          # Verify it's running
exit                      # Disconnect safely


# DAILY ROUTINE
# -------------
ssh user@server
cd ~/projects/experiments
./rj_quick_check
exit


# AFTER COMPLETION
# ----------------
./rj_check_status                                          # Final status
cd ~/projects/experiment1
tar -czf ../results_$(date +%Y%m%d).tar.gz .              # Archive
# From local machine:
scp user@server:~/projects/results_*.tar.gz .             # Download
