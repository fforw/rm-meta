#version 300 es
precision lowp float;

uniform float u_time;
uniform vec2 u_resolution;
uniform vec4 u_mouse;
uniform vec3 u_palette[8];
uniform float u_shiny[8];
uniform vec3 u_background[30];

const float pi = 3.141592653589793;
const float tau = pi * 2.0;
const float hpi = pi * 0.5;
const float qpi = pi * 0.25;
const float phi = (1.0+sqrt(5.0))/2.0;

out vec4 outColor;


#define MAX_STEPS 100
#define MAX_DIST 150.
#define SURF_DIST .001

#define ROT(a) mat2(cos(a), -sin(a), sin(a), cos(a))
#define SHEARX(a) mat2(1, 0, sin(a), 1)

float rand(float n){return fract(sin(n) * 43758.5453123);}

// Camera helper

vec3 Camera(vec2 uv, vec3 p, vec3 l, float z) {
    vec3 f = normalize(l-p),
    r = normalize(
    cross(
    vec3(0, 1, 0),
    f
    )
    ),
    u = cross(f, r),
    c = p + f * z,
    i = c + uv.x*r + uv.y*u,
    d = normalize(i-p);
    return d;
}


// 2d rotation matrix helper
mat2 Rot(float a) {
    float x = cos(a);
    float y = sin(a);
    return mat2(x, -y, y, x);
}

// RAY MARCHING PRIMITIVES

float smin(float a, float b, float k) {
    float h = clamp(0.5+0.5*(b-a)/k, 0., 1.);
    return mix(b, a, h) - k*h*(1.0-h);
}

float sdSphere(vec3 p, float s)
{
    return length(p)-s;
}

vec2 opUnion(vec2 curr, float d, float id)
{
    if (d < curr.x)
    {
        curr.x = d;
        curr.y = id;
    }

    return curr;
}

vec2 softMinUnion(vec2 curr, float d, float id)
{
    if (d < curr.x)
    {
        curr.x = smin(curr.x, d, 0.5);
        curr.y = id;
    }

    return curr;
}


float shape(float v, float x)
{
    return x > 0.0 ? -abs(v) : abs(v);
}

vec2 getDistance(vec3 p) {

    float t = u_time;

    float resultA = (sdSphere(p - vec3(sin(t) * 2.0,-2,0), 2.0) + sdSphere(p - vec3(-sin(t) * 2.0,-2,0), 2.0));

    float resultB = smin(sdSphere(p - vec3(sin(t) * 2.0,2,0), 2.0), sdSphere(p - vec3(-sin(t) * 2.0,2,0), 2.0), 0.5);
    return opUnion(vec2(resultA, 2.0), resultB, 3.0);

}


vec3 rayMarch(vec3 ro, vec3 rd) {


    float dO = 0.;
    float id = 0.0;

    int i;

    for (i=0; i < MAX_STEPS; i++) {
        vec3 p = ro + rd*dO;
        vec2 result = getDistance(p);
        float dS = result.x;
        dO += dS;
        id = result.y;
        if (dO > MAX_DIST || abs(dS) < SURF_DIST * (dO*.125 + 1.))
        break;
    }

    return vec3(dO, id, i);
}

vec3 getNormal(vec3 p) {
    float d = getDistance(p).x;
    vec2 e = vec2(.001, 0);

    vec3 n = d - vec3(
    getDistance(p-e.xyy).x,
    getDistance(p-e.yxy).x,
    getDistance(p-e.yyx).x
    );

    return normalize(n);
}


vec3 getPaletteColor(float id)
{
    int last = u_palette.length() - 1;
    //return id < float(last) ? mix(u_palette[int(id)], u_palette[int(id) + 1], fract(id)) : u_palette[last];
    return mix(u_palette[int(id)], u_palette[int(id) + 1], fract(id));
}

vec3 getBackground(in vec3 n)
{
    vec3 tb = vec3(0, n.y > 0.0 ? 1 : -1 , 0);
    vec3 tbCol = u_background[(n.y > 0.0 ? 5 : 4)];

    vec3 rl = vec3(n.x > 0.0 ? 1 : -1 , 0, 0);
    vec3 rlCol = u_background[(n.x > 0.0 ? 0 : 2)];

    vec3 fb = vec3(0, 0, n.z > 0.0 ? 1 : -1);
    vec3 fbCol = u_background[(n.z > 0.0 ? 1 : 3)];


    float a1 = length(cross(n, tb));
    float a2 = length(cross(n, rl));
    float a3 = length(cross(n, fb));

    return tbCol * (1.0 - a1) + rlCol * (1.0 - a2) + fbCol * (1.0 - a3);
}


void main(void)
{
    vec2 uv = (gl_FragCoord.xy-.5*u_resolution.xy)/u_resolution.y;

    vec2 m = u_mouse.xy/u_resolution.xy;

    vec3 ro = vec3(
        0,
        0,
        -20
    );

    ro.yz *= Rot((-m.y + 0.5) * pi);
    ro.xz *= Rot((-m.x + 0.5) * 7.0);

    vec3 lookAt = vec3(0, 1, 0);

    vec3 rd = Camera(uv, ro, lookAt, 1.5);

    float t = u_time * 0.025;

    float pos = fract(t * 4.0);
    float id = mod(floor(t * 4.0), 4.0);
    vec3 col = getBackground(rd);

    vec3 result = rayMarch(ro, rd);
    float d = result.x;

    vec3 p = ro + rd * d;
    if (d < MAX_DIST) {

        vec3 lightPos = vec3(-4,4,-4);
        //vec3 lightPos = vec3(-40,20,0);
        vec3 lightDir = normalize(lightPos - p);
        vec3 norm = getNormal(p);

        vec3 lightColor = vec3(1.0);

        float id = result.y;

        // ambient
        vec3 ambient = lightColor * vec3(0.001,0.005,0.01);

        // diffuse
        float diff = max(dot(norm, lightDir), 0.0);
        vec3 tone = getPaletteColor(id);

        //float shadow = softshadow(p, lightDir, 2.0);
        //vec3 diffuse = lightColor * pow(vec3(shadow),vec3(1.0,1.2,1.5)) * (diff * tone);


        // specular
        vec3 viewDir = normalize(rd);
        vec3 reflectDir = reflect(lightDir, norm);
        float spec = pow(max(dot(viewDir, reflectDir), 0.0), u_shiny[int(id)]);
        //float shadow = softshadow(p, lightDir, 10.0, length(lightPos - p));
        vec3 specular = lightColor * spec * vec3(0.7843,0.8823,0.9451) * 0.5;

        vec3 ref = getBackground(reflect(rd, norm));
        vec3 diffuse = diff * tone;

        col = ref * 0.5 + ambient + (diffuse + specular) ;

    }
    //col = applyFog(col, d, ro, rd, p);

    col = pow(col, vec3(1.0/2.2));

    outColor = vec4(
        col,
        1.0
    );

    //outColor = vec4(1,0,1,1);
}
