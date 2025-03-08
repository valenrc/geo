import requests
import os
import concurrent.futures

# Script para descargar contenido del archivo links.txt que contiene urls del ign

def download_file(url, filename):
    # Crear la carpeta de descargas si no existe
    script_dir = os.path.dirname(os.path.abspath(__file__))
    geojson_dir = os.path.join(script_dir, "..", "data", "geojson")  # Navigate to data/geojson
    os.makedirs(geojson_dir, exist_ok=True)
    
    # Asegurar que el archivo tenga la extensión .geojson
    if not filename.endswith(".geojson"):
        filename += ".geojson"
    
    filepath = os.path.join(geojson_dir, filename)
    
    # Verificar si el archivo ya existe
    if os.path.exists(filepath):
        return
    
    # Realizar la solicitud GET
    response = requests.get(url, stream=True)
    response.raise_for_status()  # Lanza una excepción si hay un error en la respuesta
    
    # Guardar el archivo con el nombre especificado
    with open(filepath, 'wb') as file:
        for chunk in response.iter_content(chunk_size=8192):
            file.write(chunk)
    
    print(f"Archivo descargado: {filename}")

def download_from_txt(txt_filename):
    with open(txt_filename, 'r') as file:
        lines = file.readlines()
    
    download_tasks = []
    for i in range(0, len(lines), 2):
        filename = lines[i].strip()
        url = lines[i+1].strip()
        download_tasks.append((url, filename))
    
    # Usar ThreadPoolExecutor para descargar los archivos en paralelo
    with concurrent.futures.ThreadPoolExecutor() as executor:
        futures = {executor.submit(download_file, url, filename) for url, filename in download_tasks}
        concurrent.futures.wait(futures)

def check_connection(url):
    try:
        response = requests.get(url, timeout=5)
        response.raise_for_status()
        return True
    except requests.RequestException as e:
        print(f"Error al conectar con {url}: {e}")
        return False

if __name__ == "__main__":
    txt_filename = "links.txt"
    if check_connection("https://wms.ign.gob.ar"):
      download_from_txt(txt_filename)
