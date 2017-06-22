import subprocess
import sys
import re

content = []
for line in sys.stdin:
    l = line.rstrip()
    if l != '':
        content.append(l)


# with open("test.out") as f:
#     content = f.readlines()
# # you may also want to remove whitespace characters like `\n` at the end of each line
# content = [x.strip() for x in content]


label_pattern = re.compile('(\[\s*(\d+):\]):')


cfg = []
fwd = []
cfg_dot = []
properties = {}
prop = None
target = None
result = ""

for l in content:
    if l == "CFG_DOT:":
        target = cfg_dot
    elif l == "CFG:":
        target = cfg
    elif l == "Forwad Analysis:":
        target = cfg
    elif l.startswith("Property: "):
        prop = {}
        properties[l[10:]] = prop
    elif label_pattern.match(l):
        target = []
        label = label_pattern.match(l).group(2)
        prop[label] = target
    elif l.startswith("Analysis Result: "):
        result = l
    elif l.strip() == "":
        # ignore
        l.strip()
    elif target is None:
        print("Invalid Input Format")
        sys.exit(-1)
    else:
        target.append(l)


cfg_dot = "\n".join([s for s in cfg_dot])

dot_process = subprocess.Popen(["dot", "-Tsvg"], stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.STDOUT)
cfg_dot_svg = dot_process.communicate(bytes((cfg_dot + "\n").encode()))[0].decode()
dot_process.terminate()

trees = {}
for p in properties:
    trees[p] = {}
    for l in properties[p]:
        inv = properties[p][l]
        if inv[-1].startswith("DOT:"):
            dot_graph = inv[-1][5:]
            del inv[-1]
            try:
                dot_process = subprocess.Popen(["dot", "-Tsvg"], stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.STDOUT)
                svg_graph = dot_process.communicate(bytes((dot_graph + "\n").encode()))[0]
                dot_process.terminate()
                trees[p][l] = svg_graph.decode()
            except:
                trees[p][l] = "Missing Graphviz installation"



print("""

<!DOCTYPE html>
<html>
<head>
    <title>FuncTion Static Analyzer Result</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">

    <!-- Bootstrap core CSS -->
    <link href="http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.4/css/bootstrap.min.css" rel="stylesheet" media="screen">

    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
    <script src="http://cdnjs.cloudflare.com/ajax/libs/html5shiv/3.7.2/html5shiv.js"></script>
    <script src="http://cdnjs.cloudflare.com/ajax/libs/respond.js/1.4.2/respond.js"></script>
    <![endif]-->
    <style>
    svg g.node {
        pointer-events: all; 
        cursor: hand;
    }
    .property_node {
        font-family: monospace; 
        position: absolute;
        border: 2px solid black;
        background-color: #eeeeee;
        display:none;    
    }
    
    </style>
</head>
<body>

""")

print(""" 
<div class="container-fluid">
    <div class="row">
        <h1>{0}</h1>
    </div>

    <div class="row">
""".format(result))

# Tab header ----------------------------------------------------------
print(""" <ul class="nav nav-tabs" role="tablist"> """)

for id,p in enumerate(properties.keys()):
    print("""
      <li role="presentation"><a href="#{0}" class="tab_button" id="tab_{0}" aria-controls="home" role="tab" data-toggle="tab">{1}</a></li>  
    """.format(id,p))

print(""" </ul> """)

# ------------------------------------------------------------------------


# Tab contents -------------------------------------------------------------

print(""" <div class="tab-content"> """)

for id,p in enumerate(properties.keys()):
    print(""" <div role="tabpanel" class="tab-pane active" id="{0}"> """.format(id))

    print("""<div class="cfg"> """)
    print(cfg_dot_svg)
    print(""" </div > """)



    print("""<div class="properties"> """)
    for node in properties[p]:
        print("""<div class="property_node" id={0}> """.format(str(id) + "_" + str(node)))
        print("<br/>".join(properties[p][node]))
        print(""" </div > """)

    print(""" </div > """)



    print(""" </div > """)



print(""" </div > """)



print("""
    </div>
</div>




<!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
<script src="http://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>
<!-- Include all compiled plugins (below), or include individual files as needed -->
<script src="http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.4/js/bootstrap.min.js"></script>

<script>


      $(function () { 
          $('.tab_button').last().click()
          $('.property_node').hide()
          
          
          $('.tab-pane').each(function() { 

                var propertyId = this.id;
                console.log(this.id) 
                $(this).find('svg g.node title').each(function() {
                    var title = $(this);
                    var nodeId = title.text();
                    var clickHandler = function(e) {
                        console.log('Clicked: ' + propertyId + '_' + nodeId);
                        var nodeState = $('div.properties div.property_node#'+ propertyId + '_' + nodeId)
                        nodeState.css('top', e.pageY + 15);
                        nodeState.css('left', e.pageX + 15);
                        nodeState.toggle();
                    };
                    title.parent().on('click', clickHandler);
                    
                })


          })
          
          
      })
      
  </script>
</body>
</html>


""")
