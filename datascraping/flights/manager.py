import asyncio
import scrape
import time
import random
import sys
import subprocess

codeList = ["London - Custard", "London - Crumpets", "London - Biscuit"]#["Manchester - United", "Manchester - City", "Edinburgh - Keeper Willie",
server = 0
switch = 0
total_process_time = 0
total_tasks_processed = 0

random.shuffle(codeList)

headless = sys.argv[1].lower() == 'true' if len(sys.argv) > 1 else False
sephamore = int(sys.argv[2]) if len(sys.argv) > 2 else 10

print(f"Headless mode: {headless}")
print(f"Sephamores: {sephamore}")

jobs = scrape.load_jobs().to_dict(orient="records")
# jobs.reverse()

def connect_vpn():
    global server, switch
    print("")
    switch = 0
    location = codeList[server]
    print(f"Connecting to VPN location {location}…")

    try:
        subprocess.run("windscribe disconnect", shell=True, check=True,
                       capture_output=True, text=True)
    except subprocess.CalledProcessError as e:
        msg = e.stderr or e.stdout or ""
        if "Already disconnected" not in msg:
            print(f"\033[92mError disconnecting\033[0m: {msg.strip()}")
            return  # stop only if it's a real error

    time.sleep(5)

    try:
        subprocess.run(f"windscribe connect {location}", shell=True, check=True,
                    capture_output=True, text=True)
        print(f"\033[92mSuccessfully connected\033[0m to: {location}")
    except subprocess.CalledProcessError as e:
        print(f"\033[91mError connecting\033[0m to {location}:")
        print(e.stderr or e.stdout)
    finally:
        server = (server + 1) % len(codeList)
    print("")

connect_vpn()

while len(jobs) > 0:

    tasks = random.randint(40, 45)
    if tasks > len(jobs):
        tasks = len(jobs)

    print(f"Starting scrape for {tasks} tasks...")

    start_time = time.perf_counter()
    successful = asyncio.run(scrape.main(jobs[:tasks], sephamore, headless))
    end_time = time.perf_counter()
    elapsed_seconds = end_time - start_time
    elapsed_minutes = round(elapsed_seconds / 60, 2)
    
    print(f"\nScraping completed in {elapsed_minutes} mins")

    print(f"Total Successful Scrapes: {successful}/{tasks}\n")

    jobs = scrape.load_jobs().to_dict(orient="records")
    jobs.reverse()

    # if successful/tasks < 0.8:
    #     connect_vpn()
    if successful/tasks < 0.8 or tasks - successful >= 3:
        connect_vpn()
    # if switch == random.randint(2,4):
    #     connect_vpn()

    sleep_duration = random.randint(30, 60)

    if successful/tasks < 0.6 or tasks - successful >= 10:
        sleep_duration = random.randint(60*4, 60*6)

    print(f"\nSleeping for {str(round(sleep_duration, 2))} seconds...")

    batch_cycle_time = elapsed_seconds + sleep_duration
    
    total_process_time += batch_cycle_time
    total_tasks_processed += tasks

    if total_tasks_processed > 0:
        avg_mins_per_task = total_process_time / total_tasks_processed
        
        remaining_jobs_count = len(jobs)
        est_mins_remaining = remaining_jobs_count * avg_mins_per_task
        
        est_hours = int(est_mins_remaining // 60)
        est_mins = int(est_mins_remaining % 60)
        
        print(f"\nAvg pace: {avg_mins_per_task:.3f} mins/task")
        print(f"Jobs remaining: {remaining_jobs_count}")
        # print(f"Est. time left: {round(est_hours/60, 0)}h {round(est_mins/60, 0)}m\n")
        print(f"Est. time left: {est_hours}h {est_mins}m\n")

    time.sleep(sleep_duration)
    switch += 1