#!/usr/bin/env python3

def generate_tic_tac_toe_icon():
    # SVG dimensions
    width = 100
    height = 100
    viewbox = f"0 0 {width} {height}"
    
    # Grid calculations - 3x3 grid with equal spacing
    margin = 20  # Space from edge to grid
    grid_width = width - 2 * margin  # 60
    cell_size = grid_width / 3  # 20
    
    # Grid line positions
    v1 = margin + cell_size  # 40
    v2 = margin + 2 * cell_size  # 60
    h1 = margin + cell_size  # 40  
    h2 = margin + 2 * cell_size  # 60
    
    # Cell center positions
    cell_centers = [
        (margin + cell_size/2, margin + cell_size/2),      # top-left
        (margin + 3*cell_size/2, margin + cell_size/2),    # top-center
        (margin + 5*cell_size/2, margin + cell_size/2),    # top-right
        (margin + cell_size/2, margin + 3*cell_size/2),    # center-left
        (margin + 3*cell_size/2, margin + 3*cell_size/2),  # center
        (margin + 5*cell_size/2, margin + 3*cell_size/2),  # center-right
        (margin + cell_size/2, margin + 5*cell_size/2),    # bottom-left
        (margin + 3*cell_size/2, margin + 5*cell_size/2),  # bottom-center
        (margin + 5*cell_size/2, margin + 5*cell_size/2),  # bottom-right
    ]
    
    # X mark size
    x_size = 6
    x_offset = x_size / 2
    
    # Circle radius
    circle_radius = 4
    
    svg_content = f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="{viewbox}" width="{width}" height="{height}">
  <!-- Background circle -->
  <circle cx="50" cy="50" r="45" fill="#667eea" stroke="#764ba2" stroke-width="2"/>
  
  <!-- Grid lines -->
  <g stroke="#ffffff" stroke-width="3" fill="none">
    <line x1="{v1}" y1="{margin}" x2="{v1}" y2="{width-margin}"/>
    <line x1="{v2}" y1="{margin}" x2="{v2}" y2="{width-margin}"/>
    <line x1="{margin}" y1="{h1}" x2="{width-margin}" y2="{h1}"/>
    <line x1="{margin}" y1="{h2}" x2="{width-margin}" y2="{h2}"/>
  </g>
  
  <!-- X marks (red) -->
  <g stroke="#e74c3c" stroke-width="4" fill="none" stroke-linecap="round">'''
    
    # Add X marks for positions 0, 6, 8 (top-left, bottom-left, bottom-right)
    x_positions = [0, 6, 8]
    for pos in x_positions:
        cx, cy = cell_centers[pos]
        x1, y1 = cx - x_offset, cy - x_offset
        x2, y2 = cx + x_offset, cy + x_offset
        svg_content += f'''
    <line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}"/>
    <line x1="{x2}" y1="{y1}" x2="{x1}" y2="{y2}"/>'''
    
    svg_content += '''
  </g>
  
  <!-- O marks (blue circles) -->'''
    
    # Add circles for positions 2, 4, 7 (top-right, center, bottom-center)
    o_positions = [2, 4, 7]
    for pos in o_positions:
        cx, cy = cell_centers[pos]
        svg_content += f'''
    <circle cx="{cx}" cy="{cy}" r="{circle_radius}" stroke="#3498db" stroke-width="4" fill="none"/>'''
    
    svg_content += '''
</svg>'''
    
    return svg_content

if __name__ == "__main__":
    svg = generate_tic_tac_toe_icon()
    with open("icon.svg", "w") as f:
        f.write(svg)
    print("Generated icon.svg with precise grid alignment!") 