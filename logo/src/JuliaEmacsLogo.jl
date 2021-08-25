module JuliaEmacsLogo

export DrawJuliaEmacsLogo

using Luxor

JULIA_CIRCLE_RATIO = 0.75 # juliacircles default

function DrawJuliaEmacsLogo()
    Drawing(125, 125, "../julia-emacs-logo.svg")
    
    background("transparent")
    setopacity(1.0)

    # Set scales so that Julia dots and Emacs motif line up
    anchor = Point(0, 10)
    r = 28.5

    # Draw the Julia logo
    origin()
    translate(anchor)
    draw_julia_dots(radius = r)

    # Draw the Emacs inspired icon 
    origin()
    translate(purple_center(anchor = anchor, radius = r))
    draw_julia_emacs_bg(radius = r)
    draw_julia_emacs_swirl(radius = r)

    finish()
    preview()
end

function purple_center(;anchor = O, radius = 20)
    return ngon(anchor, radius / JULIA_CIRCLE_RATIO, 3, pi/6, vertices=true)[end]    
end

function draw_julia_dots(;radius = 20)
    large_radius = radius / JULIA_CIRCLE_RATIO 
    juliacircles(large_radius; outercircleratio=JULIA_CIRCLE_RATIO)
end

function draw_julia_emacs_bg(;radius = 20)

    # 45° blue-purple gradient
    edge = sind(45)*radius
    cool_purple = blend(Point(-edge, -edge), Point(edge, edge))
    addstop(cool_purple, 0, Luxor.julia_blue)
    addstop(cool_purple, 1, Luxor.julia_purple)

    # Dark purple for the outline
    dark_purple = 0.5 .* Luxor.julia_purple
    outline_offset = radius/15

    # Draw outline and fill it with the gradient
    setcolor(dark_purple)
    circle(O, radius, :fill)
    setblend(cool_purple)
    circle(O, radius - outline_offset, :fill)
end


function draw_julia_emacs_swirl(;radius = 20)

    # Emacs swirl curve
    emacs_swirl_path_raw = """
M 120.052 357.83
C 87.1185 301.88 265.917 244.763 265.917 244.763
C 265.917 244.763 85.2574 238.39 124.104 160.749
C 192.146 24.7582 347.201 124.156 347.201 124.156
C 227.787 126.658 161.397 129.155 166.09 173.245
C 170.668 216.263 317.015 217.462 319.724 244.726
C 323.015 277.863 175.162 280.268 193.054 347.402
C 210.048 411.168 396.339 346.769 396.339 346.769
C 318.015 451.698 169.723 442.214 120.052 357.83
Z
"""
    emacs_swirl_segments = parse_path_segments(emacs_swirl_path_raw)

    # Scale the curve from its native scale to target 
    native_scale = 512
    swirl_scale = (native_scale / radius) / 2.0    
    emacs_swirl_segments ./= swirl_scale

    # Build the Bézier curve from parsed, scaled segements
    emacs_bezier = BezierPath()
    for segment in emacs_swirl_segments
        push!(emacs_bezier, BezierPathSegment(segment...))
    end

    # Center    
    translate(-radius,-radius)

    # Draw curve
    sethue("white")        
    drawbezierpath(emacs_bezier, :fill)
end


# Path parsing

op_map = Dict("M" => :move, "C" => :curve, "Z" => :close)

function parse_path_segment_row(row::AbstractString)

    length(strip(row)) < 1 && return ()
    
    head, tail... = split(row; keepempty=false)
    
    values = map(x -> parse(Float64, x), tail)
    coords = reshape(values, (2,:))'
    points = map(i -> Point(coords[i,:]...), 1:size(coords)[1])
    
    return (op_map[head], points)
end

function parse_path_segments(path_segments::AbstractString)
    segments = map(row -> parse_path_segment_row(row), split(path_segments, "\n"))

    segments = filter(seg -> length(seg) > 1, segments)
    
    bezier_segments = Vector{Vector{Point}}()
    current_point = O
    
    for segment in segments
        if segment[1] == :move
            current_point = segment[2][1]
        elseif segment[1] == :curve
            push!(bezier_segments, [current_point, segment[2]...])
            current_point = segment[2][end]
        end
    end
    
    return bezier_segments
end

end # module


using .JuliaEmacsLogo
DrawJuliaEmacsLogo()

