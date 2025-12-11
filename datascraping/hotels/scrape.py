import asyncio
import time
import os
import random
from datetime import datetime, timedelta
from rebrowser_playwright.async_api import async_playwright
import pandas as pd
from pandas.errors import EmptyDataError

# Global variables
buffer = []
successful = 0
error_concurrent = 0

# Configuration constants
VIEWPORT_SIZE = {'width': 1920, 'height': 1080}
USER_AGENT_STRING = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/131.0.0.0 Safari/537.36"
)
ROUTES_PATH = "routes-tallinn.csv"
PRICES_PATH = "prices.csv"
ERROR_LOG_PATH = "error.csv"
REDIRECT_LOG_PATH = "redirect.csv"
NOHOTEL_LOG_PATH = "nohotel.csv"
BUFFER_SIZE = 5

def bring_terminal_to_front():
    os.system(
        'osascript -e \'tell application "iTerm" to activate\''
    )

def load_jobs():
    #Load and filter routes that haven't been scraped yet.
    df_all = pd.read_csv(ROUTES_PATH, usecols=["Location", "Date"])
    
    # Exclude routes that have been priced
    try:
        df_found = pd.read_csv(PRICES_PATH, usecols=["Location", "Date"])
        merged = df_all.merge(
            df_found.drop_duplicates(), 
            on=["Location", "Date"], 
            how="left", 
            indicator=True
        )
        df_all = merged[merged["_merge"] == "left_only"].drop(columns=["_merge"])
    except (FileNotFoundError, EmptyDataError):
        print("prices.csv not found or is empty.")
    
    # Exclude no flight routes
    try:
        noflight_routes = pd.read_csv(NOHOTEL_LOG_PATH, usecols=["Location", "Date"])
        merged = df_all.merge(
            noflight_routes.drop_duplicates(), 
            on=["Location", "Date"], 
            how="left", 
            indicator=True
        )
        df_all = merged[merged["_merge"] == "left_only"].drop(columns=["_merge"])
    except (FileNotFoundError, EmptyDataError):
        print("nohotel.csv not found or is empty.")
    
    if df_all.empty:
        print("All routes have been scraped!")
    else:
        print(f"Found {len(df_all)} unscraped routes...")
    
    return df_all


async def human_like_delay(min_ms=500, max_ms=2000):
    #Add randomized delays to mimic human behavior.
    delay = random.uniform(min_ms, max_ms) / 1000
    await asyncio.sleep(delay)


async def move_mouse_naturally(page):
    #Simulate natural mouse movements.
    try:
        viewport = page.viewport_size
        x = random.randint(100, viewport['width'] - 100)
        y = random.randint(100, viewport['height'] - 100)
        await page.mouse.move(x, y, steps=random.randint(10, 30))
        await asyncio.sleep(random.uniform(0.1, 0.3))
    except Exception:
        pass


async def random_scroll(page):
    #Perform random scrolling behavior.
    try:
        scroll_amount = random.randint(100, 500)
        await page.evaluate(f"window.scrollBy(0, {scroll_amount})")
        await asyncio.sleep(random.uniform(0.2, 0.5))
    except Exception:
        pass


async def write_to_buffer(data, lock):
    #Write data to buffer and flush if buffer size reached.
    global buffer, successful
    
    async with lock:
        buffer.append(data)
        successful += 1
        
        if len(buffer) >= BUFFER_SIZE:
            df = pd.DataFrame(buffer)
            df.to_csv(
                PRICES_PATH, 
                mode="a", 
                header=not os.path.exists(PRICES_PATH), 
                index=False
            )
            buffer.clear()
            print("Data stored")


async def flush_buffer(lock):
    #Flush any remaining data in buffer to CSV.
    global buffer
    
    async with lock:
        if buffer:
            df = pd.DataFrame(buffer)
            df.to_csv(
                PRICES_PATH, 
                mode="a", 
                header=not os.path.exists(PRICES_PATH), 
                index=False
            )
            buffer.clear()
            print("Data stored")


async def log_error(route, error_msg, lock):
    global error_concurrent
    #Log errors to CSV file.
    error_concurrent += 1
    if error_concurrent > 1:
        bring_terminal_to_front()
    async with lock:
        df_error = pd.DataFrame([{
            "Location": route["Location"],
            "Date": route["Date"],
            "Error": str(error_msg)
        }])
        df_error.to_csv(
            ERROR_LOG_PATH,
            mode='a',
            header=not os.path.exists(ERROR_LOG_PATH),
            index=False
        )


async def log_redirect(route, redirected_url, lock):
    #Log redirected URLs to CSV file.
    async with lock:
        df_redirect = pd.DataFrame([{
            "Location": route["Location"],
            "Date": route["Date"],
            "RedirectedURL": redirected_url,
            "Timestamp": datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        }])
        df_redirect.to_csv(
            REDIRECT_LOG_PATH,
            mode='a',
            header=not os.path.exists(REDIRECT_LOG_PATH),
            index=False
        )


async def log_no_hotel(route, url, lock):
    #Log routes with no hotel to CSV file.
    async with lock:
        df_nohotel = pd.DataFrame([{
            "Location": route["Location"],
            "Date": route["Date"],
            "URL": url,
            "Scraped": datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            
        }])
        df_nohotel.to_csv(
            NOHOTEL_LOG_PATH,
            mode='a',
            header=not os.path.exists(NOHOTEL_LOG_PATH),
            index=False
        )


async def scrape_route(context, route, semaphore, lock, error_lock, redirect_lock, 
                       noflight_lock, task_num, total_tasks):
    #Scrape a single route for flight prices.
    global error_concurrent
    async with semaphore:
        start_date = datetime.strptime(route["Date"], '%Y-%m-%d')
        end_date = start_date + timedelta(days=1)
        url = f"https://www.booking.com/searchresults.en-gb.html?ss={route['Location']}&checkin={route['Date']}&checkout={end_date.strftime("%Y-%m-%d")}&group_adults=1&no_rooms=1&group_children=0"
        print(f"({task_num}/{total_tasks}) Scraping: {url}")
        
        page = await context.new_page()
        
        try:
            await human_like_delay(1000, 3000)
            await page.goto(url, wait_until="domcontentloaded")
            await human_like_delay(2000, 4000)
            
            # current_url = page.url
            
            # # Check for redirects
            # if url not in current_url:
            #     print(f"\033[91m({task_num}/{total_tasks})\033[0m Redirect detected: {current_url}")
            #     await log_redirect(route, current_url, redirect_lock)
            #     return
            
            # Simulate human behavior
            await move_mouse_naturally(page)
            await human_like_delay(500, 1500)
            await random_scroll(page)
            await human_like_delay(1000, 2000)
            
            # Wait for flight prices or no flights message
            # flight_prices = page.locator(".e2GB-price-text")
            # no_flights = page.locator(".IVAL-title", has_text="No flights found")
            # await flight_prices.first.or_(no_flights).wait_for(timeout=30000)

            await page.locator(".b87c397a13.f2f358d1de.ab607752a2").first.wait_for(timeout=30000)
            await human_like_delay(500, 1000)

            elements = page.locator(".b87c397a13.f2f358d1de.ab607752a2")
            data_list = await elements.all_text_contents()

            scrape_date = datetime.now().strftime('%Y-%m-%d')
            prices = data_list if data_list else None

            if prices is not None:
                error_concurrent = 0
                print(f"\033[92m({task_num}/{total_tasks})\033[0m Prices found: {prices or 'N/A'}")
                
                await write_to_buffer({
                    "Location": route["Location"],
                    "Date": route["Date"],
                    "Prices": prices,
                    "Scraped": scrape_date
                }, lock)
            else:
                print(f"\033[91m({task_num}/{total_tasks})\033[0m Error: No prices found.")
            
            await human_like_delay(500, 1500)
                
        except Exception as e:
            print(f"\033[91m({task_num}/{total_tasks})\033[0m Error: {e}")
            await log_error(route, e, error_lock)
            
            # Flush buffer if there's pending data
            if buffer:
                await flush_buffer(lock)
                
        finally:
            await page.close()


async def main(routes, semaphore_limit=10, headless=False):
    #Main function to coordinate scraping tasks.
    global successful
    successful = 0
    
    if not routes:
        print("All routes already scraped!")
        return 0
    
    # Initialize locks and semaphore
    semaphore = asyncio.Semaphore(semaphore_limit)
    lock = asyncio.Lock()
    error_lock = asyncio.Lock()
    redirect_lock = asyncio.Lock()
    noflight_lock = asyncio.Lock()
    
    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=headless,
            args=[
                '--disable-blink-features=AutomationControlled',
                '--disable-dev-shm-usage',
                '--no-sandbox',
                '--disable-web-security'
            ]
        )
        
        context = await browser.new_context(
            user_agent=USER_AGENT_STRING,
            viewport=VIEWPORT_SIZE,
            locale='en-GB',
            timezone_id='Europe/London',
            geolocation={'longitude': -0.1278, 'latitude': 51.5074},
            permissions=['geolocation'],
            color_scheme='light',
            has_touch=False,
            is_mobile=False,
            java_script_enabled=True,
            extra_http_headers={
                'Accept-Language': 'en-GB,en;q=0.9',
                'Accept-Encoding': 'gzip, deflate, br',
                'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
                'DNT': '1',
                'Connection': 'keep-alive',
                'Upgrade-Insecure-Requests': '1'
            }
        )
        
        # Add anti-detection scripts
        await context.add_init_script("""
            Object.defineProperty(navigator, 'webdriver', {
                get: () => undefined
            });
            window.chrome = {
                runtime: {}
            };
            Object.defineProperty(navigator, 'plugins', {
                get: () => [1, 2, 3, 4, 5]
            });
            Object.defineProperty(navigator, 'languages', {
                get: () => ['en-GB', 'en', 'en-US']
            });
        """)
        
        # Block unnecessary resources
        # await context.route("**/*", lambda route:
        #     route.abort() if route.request.resource_type in ["image", "stylesheet", "font", "media"]
        #     else route.continue_()
        # )
        
        # Create and execute tasks
        tasks = [
            scrape_route(
                context, route, semaphore, lock, error_lock, 
                redirect_lock, noflight_lock, n + 1, len(routes)
            )
            for n, route in enumerate(routes)
        ]
        
        print(f"{len(tasks)} tasks built.")
        await asyncio.gather(*tasks)
        
        # Flush any remaining data
        await flush_buffer(lock)
        
        await context.close()
        await browser.close()
    
    return successful


if __name__ == "__main__":
    start_time = time.perf_counter()
    
    routes_to_scrape = load_jobs().to_dict(orient="records")
    successful_scrapes = asyncio.run(main(routes_to_scrape, headless=True))
    
    end_time = time.perf_counter()
    elapsed_minutes = round((end_time - start_time) / 60, 2)
    
    print(f"Scraping completed in {elapsed_minutes} mins")
    print(f"Total successful scrapes: {successful_scrapes}")