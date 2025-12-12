import Graph from "graphology";
import Sigma from "sigma";
import { NodeBorderProgram } from "@sigma/node-border";
import * as d3 from "d3";

const container = document.getElementById("sigma-container");
const infoBox = document.getElementById("node-info");
const sliderPointsContainer = document.getElementById("slider-points");
const windowDisplay = document.getElementById("window-display");
const toggleBtn = document.getElementById("toggle-labels");

const graph = new Graph();
const state = { selectedNode: null, selectedNeighbors: null };
let labelsVisible = true; // track label visibility


let bertopicData = [];

d3.csv("public/bertopic_topics.csv").then(data => {
  // Convert numeric columns
  bertopicData = data.map(d => ({
    field: d.field_1,
    topic: d.topic,
    keywords: d.topic_keywords,
    N: +d.N,
    N_rel: +d.N_rel
  }));
});

// -------------------------
// Field → color mapping
//const fieldColorsResp = await fetch("public/field_color.json");
//const fieldColorMapArray = await fieldColorsResp.json();
//const fieldColorMap = {};
//fieldColorMapArray.forEach(d => fieldColorMap[d.field] = d.color);

//function renderGlobalLegend() {
 // const legendContainer = document.getElementById("legend-items");
  //legendContainer.innerHTML = "";
  //Object.entries(fieldColorMap).forEach(([field, color])=>{
   // const item = document.createElement("div");
    //item.style.display="flex"; item.style.alignItems="center"; item.style.marginBottom="6px";

    //const colorBox = document.createElement("span");
    //colorBox.style.display="inline-block";
    //colorBox.style.width="20px"; colorBox.style.height="20px";
    //colorBox.style.backgroundColor=color; colorBox.style.marginRight="10px";
    //colorBox.style.border="1px solid #000"; colorBox.style.flexShrink="0";
    //item.appendChild(colorBox);

    //const lbl = document.createElement("span");
    //lbl.textContent=field;
    //item.appendChild(lbl);
    //legendContainer.appendChild(item);
  //});
//}


function renderDonutChart(fieldName) {
  const container = d3.select("#topic-donut");
  container.selectAll("*").remove(); // clear old chart

  const minNrel = 0.05; // threshold
  const fieldTopics = bertopicData.filter(d => d.field === fieldName && d.N_rel >= minNrel);
  if (fieldTopics.length === 0) {
    container.append("div").text("No BERTopic data for this field.");
    return;
  }

  const width = 250;
  const height = 250;
  const radius = Math.min(width, height) / 2;

  const svg = container
    .append("svg")
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", `translate(${width / 2},${height / 2})`);

  const color = d3.scaleOrdinal()
    .domain(fieldTopics.map(d => d.topic))
    .range(d3.schemeCategory10);

  const pie = d3.pie()
    .sort(null)
    .value(d => d.N_rel);

  const arc = d3.arc()
    .innerRadius(radius * 0.5)
    .outerRadius(radius * 0.9);

  const tooltip = d3.select("#donut-tooltip");

  svg.selectAll("path")
    .data(pie(fieldTopics))
    .enter()
    .append("path")
    .attr("d", arc)
    .attr("fill", d => color(d.data.topic))
    .attr("stroke", "white")
    .style("stroke-width", "2px")
    .on("mouseover", (event, d) => {
      tooltip.style("display", "block")
             .html(`<b>${d.data.keywords}</b>`);
    })
    .on("mousemove", (event) => {
      tooltip.style("left", (event.pageX + 10) + "px")
             .style("top", (event.pageY + 10) + "px");
    })
    .on("mouseout", () => {
      tooltip.style("display", "none");
    });

  // Optional: small labels
  svg.selectAll("text")
    .data(pie(fieldTopics))
    .enter()
    .append("text")
    .text(d => d.data.topic)
    .attr("transform", d => `translate(${arc.centroid(d)})`)
    .style("font-size", "9px")
    .style("text-anchor", "middle");
}





// -------------------------
// Sigma setup
const renderer = new Sigma(graph, container, {
  renderLabels: true,
  nodeProgramClasses: { border: NodeBorderProgram }
});

// Force all labels to be always rendered
renderer.setSetting("labelRenderedSizeThreshold", 0);

renderer.setSetting("nodeReducer", (node, data) => {
  // Keep the label even if the node is small or outside selection
  if(state.selectedNeighbors && !state.selectedNeighbors.has(node)) {
    return { ...data, color:"#f6f6f6" }; // remove only color dimming
  }
  return data;
});

renderer.setSetting("edgeReducer", (edge, data)=>{
  if(state.selectedNeighbors && state.selectedNode && !graph.hasExtremity(edge, state.selectedNode)) return { ...data, hidden:true};
  return data;
});

// Node click
renderer.on("clickNode", ({ node })=>{
  if(state.selectedNode===node){
    state.selectedNode=null;
    state.selectedNeighbors=null;
    infoBox.style.display="none";
    d3.select("#topic-donut").selectAll("*").remove();
  } else {
    state.selectedNode=node;
    state.selectedNeighbors=new Set(graph.neighbors(node));
    state.selectedNeighbors.add(node);
    const n = graph.getNodeAttributes(node);
    const nodeDetails = document.getElementById("node-details");
    nodeDetails.innerHTML=`
<div><b>Node Information</b></div>
<div class="field"><span class="label">Field:</span><span class="value">${n.id ?? ""}</span></div>
<div class="field"><span class="label">Citations:</span><span class="value">${n.citations ?? ""}</span></div>
<div class="field"><span class="label">Nodes:</span><span class="value">${n.numbernodes ?? ""}</span></div>
<div class="field"><span class="label">Degree:</span><span class="value">${n.degree ?? ""}</span></div>
<div class="field"><span class="label">Betweeness:</span><span class="value">${n.betweeness ?? ""}</span></div>
<div class="field"><span class="label">Closeness:</span><span class="value">${n.closeness ?? ""}</span></div>
<div class="field"><span class="label">Edges (Between):</span><span class="value">${n.between_edges ?? ""}</span></div>
<div class="field"><span class="label">Edges (Within):</span><span class="value">${n.withinedges ?? ""}</span></div>
<div class="field"><span class="label">Edges (Total):</span><span class="value">${n.totaledges ?? ""}</span></div>
<div class="field"><span class="label">External Edge Ratio:</span><span class="value">${n.externalratio ?? ""}</span></div>
<div class="field"><span class="label">Internal Edge Ratio:</span><span class="value">${n.internalratio ?? ""}</span></div>`;
    infoBox.style.display="block";
    renderDonutChart(n.id);
  }
  renderer.refresh();
});

// Stage click
renderer.on("clickStage", ()=>{
  state.selectedNode=null;
  state.selectedNeighbors=null;
  infoBox.style.display="none";
  renderer.refresh();
});

function weightToGreyHex(weight, maxWeight) {
  const t = maxWeight > 0 ? Math.min(Math.max(weight / maxWeight, 0), 1) : 0;
  const v = Math.round(250 * (1 - t)); // light grey 200 → black 0
  return `#${((1 << 24) + (v << 16) + (v << 8) + v).toString(16).slice(1)}`;
}


// -------------------------
// Load window
async function loadWindow(windowIndex){

const firstJsonIndex = 1;

const fileIndex = windowIndex + firstJsonIndex;

const nodes = await fetch(`public/windows_field_aggregate_fields/nodes_window_${String(fileIndex).padStart(3,"0")}.json`)
  .then(r => r.json());

const edges = await fetch(`public/windows_field_aggregate_fields/edges_window_${String(fileIndex).padStart(3,"0")}.json`)
  .then(r => r.json());

const maxWeight = edges.reduce((max, e) => Math.max(max, e.weight ?? 0), 0);

  graph.clear();

  nodes.forEach(n=>{
    graph.addNode(n.id,{
      ...n,
      x: typeof n.x==="number" ? n.x : Math.random(),
      y: typeof n.y==="number" ? n.y : Math.random(),
      size: n.size_nodes,
      color: n.color ?? "#666",
      type:"border",
      borderColor:"#000",
      label: labelsVisible ? (n.label?.length>30 ? n.label.slice(0,30)+"…" : n.label) : "",
      fullTitle: n.label,
      opacity:1
    });
  });

edges.forEach(e => {
  const w = Number(e.weight) || 0;
  graph.addEdge(e.source, e.target, {
    ...e,
    color: weightToGreyHex(w, maxWeight),
    size: 1// optional thickness scaling
  });
});



  windowDisplay.textContent = nodes.length>0 && nodes[0].window ? nodes[0].window : `Window ${windowIndex+1}`;
  highlightSliderPoint(windowIndex);
  renderer.refresh();

const nodeDisplay = document.getElementById("node-display");
if (nodeDisplay) {
  nodeDisplay.textContent = `${nodes.length} Nodes`;
}

const edgeDisplay = document.getElementById("edge-display");
if (edgeDisplay) {
  edgeDisplay.textContent = `${edges.length} Edges`;
}


}

// -------------------------
// Discrete slider points
const totalWindows = 41;

function renderSliderPoints() {
  sliderPointsContainer.innerHTML = "";
  for (let i=0; i<totalWindows; i++){
    const point = document.createElement("div");
    point.className = "slider-point";
    point.style.left = (i/(totalWindows-1))*100 + "%";

    // Tick labels every 5 points
    if(i%5===0){
      const tickLabel = document.createElement("div");
      tickLabel.className="tick-label";
      tickLabel.textContent = `${1980+i}-${1980+i+4}`;
      point.appendChild(tickLabel);
    }

    point.addEventListener("click", ()=> loadWindow(i));
    sliderPointsContainer.appendChild(point);
  }
}

function highlightSliderPoint(idx){
  const points = sliderPointsContainer.children;
  for (let i=0; i<points.length; i++){
    points[i].classList.toggle("active", i===idx);
  }
}

// -------------------------
// Toggle Labels Button
toggleBtn.addEventListener("click", () => {
  labelsVisible = !labelsVisible;
  renderer.refresh(); // redraw nodes with or without labels
  // Force reload of current window to update labels
  const activeIndex = Array.from(sliderPointsContainer.children).findIndex(p => p.classList.contains("active"));
  if(activeIndex >= 0) loadWindow(activeIndex);
});

// -------------------------
// Initialize
renderSliderPoints();
loadWindow(0);
