

var config = {
    container: "#collapsable-example",
    animateOnInit: false,
    
    // node: {
    // 	HTMLclass: "nodeExample1",
    // },
    animation: {
        nodeAnimation: "linear",
        nodeSpeed: 700,
        connectorsAnimation: "linear",
        connectorsSpeed: 600
    },
    connectors: {
	type: "curve"
    }
},

    node1 = {
        image: "img/node1.png",
    },
    node1d = {
        parent: node1,
        image: "img/node1d.png",
    },
    node2 = {
        parent: node1d,
        image: "img/node2.png",
    },
    node3 = {
        parent: node1d,
        image: "img/node3.png",
    },
    node2d = {
        parent: node2,
        image: "img/node2d.png",
    },
    node3d = {
        parent: node3,
        image: "img/node3d.png",
    },
    node4 = {
        parent: node2d,
        image: "img/vdots.png",
	HTMLclass: "highlight"	
    },
    node5 = {
        parent: node2d,
        image: "img/vdots.png",
	HTMLclass: "highlight"	
    },
    node6 = {
        parent: node3d,
        image: "img/vdots.png",
	HTMLclass: "highlight"	
    },
    node7 = {
        parent: node3d,
        image: "img/vdots.png",
	HTMLclass: "highlight"	
    },    
    node8 = {
        parent: node7,
        image: "img/terminal.png",
    },
    node9 = {
        parent: node7,
        image: "img/vdots.png",
    },
    


    chart_config = [config, node1, node1d, node2, node3, node2d, node3d, node4, node5, node6, node7, node8, node9];

