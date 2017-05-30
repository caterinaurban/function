import subprocess
import sys
import re

content = []
for line in sys.stdin:
    l = line.rstrip()
    if l != '':
        content.append(l)

label_pattern = re.compile('(\[\s*\d+:\]):')


statements = []
properties = {}
prop = None
target = None
result = ""

for l in content:
    if l == "Abstract Syntax:":
        target = statements
    elif l.startswith("Property: "):
        prop = {}
        properties[l[10:]] = prop
    elif label_pattern.match(l):
        target = []
        label = label_pattern.match(l).group(1)
        prop[label] = target
    elif l.startswith("Analysis Result: "):
        result = l
    elif target is None:
        print("Invalid Input Format")
        sys.exit(-1)
    else:
        target.append(l)


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
      .tooltip-inner {
            max-width: none;
            white-space: nowrap;
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


properties_formatted = {}

for id_p, k in enumerate(properties):
    properties_formatted[k] = {}
    inv = properties[k]
    for id_l, l in enumerate(inv):
        formatted_state = '<span class="label label-default" onMouseLeave="hideTrees()" onMouseEnter="showTree( '+str(id_p)+ ',' + str(id_l) +' )" data-html="true" data-container="body" data-placement="right" data-toggle="tooltip" title="<div style=\'text-align:left;font-size:12pt;\'>' \
                          + "<br/>".join(inv[l]) + '</div>">' + l + '</span>'
        properties_formatted[k][l] = formatted_state


# print(properties_formatted)

# Tab content -------------------------------------------------------------

print(""" <div class="tab-content"> """)

for id,p in enumerate(properties.keys()):
    formatted_property = properties_formatted[p]
    print(""" <div role="tabpanel" class="tab-pane active" id="{0}"> """.format(id))
    print(""" <div class="row"> """)



    print("""
                       <div class="col-md-4">
                        <pre>
    """)
    for s in statements:
        formatted_stmt = s
        for l in formatted_property:
            formatted_stmt = formatted_stmt.replace(l, formatted_property[l])
        print(formatted_stmt)
        # print(s)
    print("""</pre>""")
    print(""" </div > """)


    print(""" <div class="col-md-8"> """)

    for id_l, l in enumerate(trees[p]):
        print("""<div class="decision_tree tree_{0}_{1}">  """.format(id, id_l))
        print(trees[p][l])
        print("""</div>""")

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

        function hideTrees() {
            $('div.decision_tree').hide();
        }
        
        function showTree(pid,lid) {
            $('div.tree_'+pid+'_'+lid).show();
         }

      $(function () { 
          $('[data-toggle="tooltip"]').tooltip() 
          $('.tab_button').last().click()
          hideTrees();
      })
      
  </script>
</body>
</html>


""")



